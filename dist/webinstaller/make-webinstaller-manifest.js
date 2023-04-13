const fs = require('fs');

const [, , ...args] = process.argv;
const [baseUriArg, BaseURI, releaseDateArg, releaseDate, outputArg, outputFile] = args;

function validateArg(args, argName, regex) {
    const argIndex = args.indexOf(argName);
    if (argIndex < 0) {
      console.log(`error: missing ${argName}`);
      return false;
    }
  
    const argValue = args[argIndex + 1];
    if (!regex.test(argValue)) {
      console.log(`error: invalid ${argName}; must match regular expression: ${regex}`);
      return false;
    }
  
    return true;
  }
  
  if (!validateArg(args, '-BaseURI', /^https?:\/\/.*\/$/)) {
    return 0;
  }
  
  if (!validateArg(args, '-releaseDate', /^\d{4}-\d{2}-\d{2}$/)) {
    return 0;
  }
  

//get the version
const releasesRegex = /\/(\d+\.\d+\.\d+)\/$/;
const releasesVersion = BaseURI.match(releasesRegex);

const template = fs.readFileSync('quick-lint-js-template.json', 'utf8');

const data = JSON.parse(template);

// set version and insert download with the approriate value 
for (const release of data.release) {
    release.version = releasesVersion[1];
    release.download = release.download.replace('${version}', releasesVersion[1]);
    release.date = releaseDate;
}

// Write the modified data back to a file
fs.writeFileSync(outputFile, JSON.stringify(data, null, 2), 'utf8');
