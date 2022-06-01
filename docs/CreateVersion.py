import requests
import re
import sys

def main():
    CHANGELOG_REQ_LINK = "https://raw.githubusercontent.com/quick-lint/quick-lint-js/master/docs/CHANGELOG.md"
    VERSION_PUSH_LINK = "https://c.quick-lint-js.com/releases/" #add version/README
    AUTH_TOKEN = "AUTH_TOKEN_HERE" #Careful!

    r = requests.get(CHANGELOG_REQ_LINK)
    entireChangelog = r.text

    newQlVersionLine, oldQlVersionLine = getQlVersionLines(entireChangelog)
    newQlVersion = getQlVersion(newQlVersionLine)
    changelog = getQlVersionChanges(entireChangelog, newQlVersionLine, oldQlVersionLine)
    changelog += getImbeds(entireChangelog)
    officialPush = getOfficialPush(VERSION_PUSH_LINK, newQlVersion)
    
    createVersion(AUTH_TOKEN, newQlVersion, changelog, officialPush)

def createVersion(AUTH_TOKEN, ql_version, changelog, pushID):
    headers = {"Authorization": "token " + AUTH_TOKEN}
    data = {
        "tag_name": ql_version,
        "name": ql_version,
        "body": changelog,
        "target_commitish": pushID
    }
    sendRequest = requests.post("https://github.com/quick-lint/quick-lint-js/releases", headers = headers, json = data)
    print("Created version of quick-lint-js " + ql_version + " of commit " + pushID)

#Returns the commit id of the new respective version of quick-lint. Gets the id from the website
def getOfficialPush(link, ql_version):
    link = link + ql_version + "/README"
    r = requests.get(link)
    returned = r.text
    s = "Git-Commit: "
    push_id = returned[returned.index(s) + len(s):]
    push_id = push_id.split("\n")[0]
    return push_id

#Retrusn the imbeds at the bottom of the changelog
def getImbeds(changelog):
    return changelog.split("## 0.2.0 (2021-04-05)\n\nBeta release.\n",1)[1]

#Returns the changelog for the newest version of quick-lint
def getQlVersionChanges(entireChangelog, startLine, endLine):
    newLog = ""
    recordLine = False
    for line in entireChangelog.splitlines():
        if line == startLine:
            recordLine = True
        if line == endLine:
            recordLine = False
        if recordLine:
            newLog += line + "\n"
    return newLog

#Returns a string of a quick-lint version from getQlVersionLines
def getQlVersion(line):
    line = line.replace(" ", "")
    version = line[line.find("#") + 2:line.find('(')]
    return version

#Returns the lines of the newest and second newest versions of quick-lint
def getQlVersionLines(data):
    versionLine = None
    secondVersionLine = None
    for line in data.splitlines():
        if re.search("^## *[0-9]+[-.][0-9]+[-.][0-9]+ *[(-][0-9]+[--][0-9]+[--][0-9]+[-)]", line):
            if versionLine == None:
                versionLine = line
                continue
            if versionLine != None:
                secondVersionLine = line
            if versionLine != None and secondVersionLine != None:
                break
    if versionLine == None or secondVersionLine == None:
        sys.exit("Error getting quick-lint version from changelog.")

    return versionLine, secondVersionLine

if __name__ == "__main__":
    main()

