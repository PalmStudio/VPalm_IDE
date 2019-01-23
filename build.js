// C:\Users\sdkca\Desktop\electron-workspace\build.js
var electronInstaller = require('electron-winstaller');

// In this case, we can use relative paths
var settings = {
  appDirectory: './VPalm_IDE/VPalm_IDE-win32-x64',
  outputDirectory: './VPalm_IDE/installer-x64',
  iconURL: 'cc.ico',
  setupIcon: 'cc.ico',
  version: '1.0.0',
  authors: "R. Vezy <remi.vezy@cirad.fr> (https://remi-vezy.netlify.com/); R. Perez <raphael.perez@cirad.fr>,  J. Dauzat <jean.dauzat@cirad.fr>"
};

resultPromise = electronInstaller.createWindowsInstaller(settings);

resultPromise.then(() => {
    console.log("The installers of your application were succesfully created !");
}, (e) => {
    console.log(`There was an error building the windows installers for the app: ${e.message}`)
});
