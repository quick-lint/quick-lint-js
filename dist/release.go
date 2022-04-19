// Copyright (C) 2020  Matthew "strager" Glazar
// See end of file for extended copyright information.

package main

import "bufio"
import "flag"
import "fmt"
import "log"
import "os"
import "os/exec"
import "strings"

type Step struct {
	Title string
	Run   func()
}

var ReleaseCommitHash string
var ReleaseVersion string

var Steps []Step = []Step{
	Step{
		Title: "Update release notes file",
		Run: func() {
			fmt.Printf("Update the release notes file: docs/CHANGELOG.md\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Update version number and release date",
		Run: func() {
			fmt.Printf("Change these files containing version numbers:\n")
			fmt.Printf("* Formula/quick-lint-js.rb\n")
			fmt.Printf("* dist/arch/PKGBUILD-dev\n")
			fmt.Printf("* dist/arch/PKGBUILD-git\n")
			fmt.Printf("* dist/arch/PKGBUILD-release\n")
			fmt.Printf("* dist/chocolatey/quick-lint-js.nuspec\n")
			fmt.Printf("* dist/chocolatey/tools/VERIFICATION.txt\n")
			fmt.Printf("* dist/debian/README.md\n")
			fmt.Printf("* dist/debian/debian/changelog\n")
			fmt.Printf("* dist/msix/AppxManifest.xml\n")
			fmt.Printf("* dist/npm/BUILDING.md\n")
			fmt.Printf("* dist/npm/package.json\n")
			fmt.Printf("* dist/scoop/quick-lint-js.template.json\n")
			fmt.Printf("* dist/sign-release.go\n")
			fmt.Printf("* dist/winget/quick-lint.quick-lint-js.installer.template.yaml\n")
			fmt.Printf("* dist/winget/quick-lint.quick-lint-js.locale.en-US.template.yaml\n")
			fmt.Printf("* dist/winget/quick-lint.quick-lint-js.template.yaml\n")
			fmt.Printf("* plugin/vim/quick-lint-js.vim/doc/quick-lint-js.txt\n")
			fmt.Printf("* plugin/vscode-lsp/README.md\n")
			fmt.Printf("* plugin/vscode-lsp/package.json\n")
			fmt.Printf("* plugin/vscode/BUILDING.md\n")
			fmt.Printf("* plugin/vscode/package.json\n")
			fmt.Printf("* version\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Re-generate man pages",
		Run: func() {
			fmt.Printf("Re-generate man pages to include the updated version number by running:\n")
			fmt.Printf("$ ./docs/man/generate-man-pages\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Create commit",
		Run: func() {
			fmt.Printf("Create a commit.\n")
			WaitForDone()
			ReleaseCommitHash = GetCurrentGitCommitHash()
		},
	},

	Step{
		Title: "Push to GitHub",
		Run: func() {
			fmt.Printf("Push your commit to GitHub on a non-main branch on https://github.com/quick-lint/quick-lint-js (not a fork).\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Wait for builds",
		Run: func() {
			fmt.Printf("Wait for all GitHub Actions workflows to finish and to succeed.\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Download builds",
		Run: func() {
			if ReleaseCommitHash == "" {
				log.Fatalf("missing -ReleaseCommitHash\n")
			}
			fmt.Printf("Download the build artifacts from the artifact server:\n")
			fmt.Printf("$ rsync -av github-ci@c.quick-lint-js.com:/var/www/c.quick-lint-js.com/builds/%s/ builds/\n", ReleaseCommitHash)
			WaitForDone()
		},
	},

	Step{
		Title: "Sign the build artifacts",
		Run: func() {
			fmt.Printf("Sign the build artifacts:\n")
			fmt.Printf("$ go run dist/sign-release.go dist/deep-hasher.go dist/appx.go -RelicConfig=dist/certificates/relic-config.yaml builds/ signed-builds/\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Create a Scoop manifest",
		Run: func() {
			fmt.Printf("Create a Scoop manifest:\n")
			fmt.Printf("$ go run ./dist/scoop/make-manifest.go -BaseURI \"https://c.quick-lint-js.com/releases/%s/\" -x86-ZIP signed-builds/manual/windows-x86.zip -x64-ZIP signed-builds/manual/windows.zip -Out signed-builds/scoop/quick-lint-js.json\n", ReleaseVersion)
			WaitForDone()
		},
	},

	Step{
		Title: "Create a winget manifest",
		Run: func() {
			fmt.Printf("Create a winget manifest:\n")
			fmt.Printf("$ go run ./dist/winget/make-manifests.go -BaseURI \"https://c.quick-lint-js.com/releases/%s/\" -MSIX signed-builds/windows/quick-lint-js.msix -OutDir signed-builds/winget/\n", ReleaseVersion)
			WaitForDone()
		},
	},

	Step{
		Title: "Upload the signed build artifacts",
		Run: func() {
			fmt.Printf("Upload the signed build artifacts to the artifact server:\n")
			fmt.Printf("$ rsync -av signed-builds/ github-ci@c.quick-lint-js.com:/var/www/c.quick-lint-js.com/releases/%s/\n", ReleaseVersion)
			WaitForDone()
		},
	},

	Step{
		Title: "Update `latest` symlink",
		Run: func() {
			fmt.Printf("Update the `latest` symlink on the artifact server:\n")
			fmt.Printf("$ ssh github-ci@c.quick-lint-js.com \"ln --force --no-dereference --symbolic %s /var/www/c.quick-lint-js.com/releases/latest\"\n", ReleaseVersion)
			WaitForDone()
		},
	},

	Step{
		Title: "Publish the Visual Studio Code extension to the Marketplace",
		Run: func() {
			fmt.Printf("With the `vscode/quick-lint-js-*.vsix` artifact:\n")
			fmt.Printf("$ npx vsce publish --packagePath signed-builds/vscode/quick-lint-js-*.vsix\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Publish the Visual Studio Code extension to the Open VSX Registry",
		Run: func() {
			fmt.Printf("With the `vscode/quick-lint-js-*.vsix` artifact:\n")
			fmt.Printf("$ npx ovsx publish signed-builds/vscode/quick-lint-js-*.vsix --pat YOUR_ACCESS_TOKEN\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Publish to npm",
		Run: func() {
			fmt.Printf("With the `npm/quick-lint-js-*.tgz` artifact:\n")
			fmt.Printf("$ npm publish signed-builds/npm/quick-lint-js-*.tgz\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Publish Debian packages",
		Run: func() {
			fmt.Printf("Run the `dist/debian/sync-releases-to-apt` script.\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Publish the website",
		Run: func() {
			if ReleaseCommitHash == "" {
				log.Fatalf("missing -ReleaseCommitHash\n")
			}
			fmt.Printf("Publish the website: Run `./website/deploy.sh %s`.\n", ReleaseCommitHash)
			WaitForDone()
		},
	},

	Step{
		Title: "Create Git tag",
		Run: func() {
			fmt.Printf("Create a Git tag named `%s`. Push it to GitHub.\n", ReleaseVersion)
			WaitForDone()
		},
	},

	Step{
		Title: "Push to master",
		Run: func() {
			fmt.Printf("Push the commit to the `master` branch on GitHub.\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Update Arch Linux user repositories (AUR)",
		Run: func() {
			fmt.Printf("1. Clone ssh://aur@aur.archlinux.org/quick-lint-js with Git.\n")
			fmt.Printf("2. Update README to point to the tag's commit.\n")
			fmt.Printf("3. Run `dist/arch/update-aur.sh --docker --test /path/to/quick-lint-js-aur-clone`.\n")
			fmt.Printf("4. Commit all files with message \"Update quick-lint-js to version %s\".\n", ReleaseVersion)
			fmt.Printf("5. Push to the `master` branch on AUR.\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Update Nixpkgs package manager",
		Run: func() {
			fmt.Printf("1. Clone https://github.com/NixOS/nixpkgs with Git.\n")
			fmt.Printf("2. Update the version number and SHA1 hash in the pkgs/development/tools/quick-lint-js/default.nix file.\n")
			fmt.Printf("3. Test installation by running `nix-env -i -f . -A quick-lint-js`.\n")
			fmt.Printf("4. Commit all files with message \"quick-lint-js: OLDVERSION -> %s\".\n", ReleaseVersion)
			fmt.Printf("5. Push to a fork on GitHub.\n")
			fmt.Printf("6. Create a pull request on GitHub.\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Update Scoop package manager",
		Run: func() {
			fmt.Printf("1. Clone https://github.com/ScoopInstaller/Main with Git.\n")
			fmt.Printf("2. Copy .../signed-builds/scoop/quick-lint-js.json to bucket/quick-lint-js.json\n")
			fmt.Printf("3. Commit all files with message \"quick-lint-js: Update to version %s\".\n", ReleaseVersion)
			fmt.Printf("4. Push to a fork on GitHub.\n")
			fmt.Printf("5. Create a pull request on GitHub.\n")
			fmt.Printf("6. On the pull request, write a comment: \"/verify\"\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Update winget package manager",
		Run: func() {
			fmt.Printf("1. Clone https://github.com/microsoft/winget-pkgs with Git.\n")
			fmt.Printf("2. Copy .../signed-builds/winget/* manifests/q/quick-lint/quick-lint-js/%s/\n", ReleaseVersion)
			fmt.Printf("3. Commit all files with message \"Add quick-lint-js version %s\".\n", ReleaseVersion)
			fmt.Printf("4. Push to a fork on GitHub.\n")
			fmt.Printf("5. Create a pull request on GitHub.\n")
			WaitForDone()
		},
	},

	Step{
		Title: "Update Chocolatey package manager",
		Run: func() {
			fmt.Printf("1. Copy signed-builds/chocolatey/quick-lint-js.nupkg to a Windows machine.\n")
			fmt.Printf("2. On the Windows machine, run: choco push quick-lint-js.nupkg --source https://push.chocolatey.org/\n")
			WaitForDone()
		},
	},
}

var ConsoleInput *bufio.Reader
var CurrentStepIndex int

func main() {
	ConsoleInput = bufio.NewReader(os.Stdin)

	startAtStepNumber := 0
	flag.IntVar(&startAtStepNumber, "StartAtStep", 1, "")
	flag.StringVar(&ReleaseCommitHash, "ReleaseCommitHash", "", "")
	flag.Parse()
	if flag.NArg() != 1 {
		fmt.Fprintf(os.Stderr, "error: expected exactly one argument (a version number)\n")
		os.Exit(2)
	}
	ReleaseVersion = flag.Arg(0)
	CurrentStepIndex = startAtStepNumber - 1

	for CurrentStepIndex < len(Steps) {
		step := &Steps[CurrentStepIndex]
		fmt.Printf("#%d: %s\n", CurrentStepIndex+1, step.Title)
		step.Run()
		fmt.Printf("\n")
		CurrentStepIndex += 1
	}
}

func WaitForDone() {
	fmt.Printf("Type 'done' when you're done: ")
retry:
	text, err := ConsoleInput.ReadString('\n')
	if err != nil {
		log.Fatal(err)
	}
	if text == "done\n" {
		return
	}
	if text == "stop\n" {
		fmt.Printf("\nStopped at step #%d\n", CurrentStepIndex+1)
		fmt.Printf("To resume, run:\n")
		fmt.Printf("$ go run dist/release.go -StartAtStep=%d", CurrentStepIndex+1)
		if ReleaseCommitHash != "" {
			fmt.Printf(" -ReleaseCommitHash=%s", ReleaseCommitHash)
		}
		fmt.Printf(" %s\n", ReleaseVersion)
		os.Exit(0)
	}
	fmt.Printf("What's that? Type 'done' or 'stop': ")
	goto retry
}

func GetCurrentGitCommitHash() string {
	cmd := exec.Command("git", "rev-parse", "@")
	cmd.Stderr = os.Stderr
	stdout, err := cmd.Output()
	if err != nil {
		log.Fatalf("failed to get Git commit hash: %v", err)
	}
	return strings.TrimSpace(string(stdout))
}

// quick-lint-js finds bugs in JavaScript programs.
// Copyright (C) 2020  Matthew "strager" Glazar
//
// This file is part of quick-lint-js.
//
// quick-lint-js is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// quick-lint-js is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with quick-lint-js.  If not, see <https://www.gnu.org/licenses/>.
