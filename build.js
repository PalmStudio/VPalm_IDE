var electronInstaller = require('electron-winstaller');

// In this case, we can use relative paths
var settings = {
  appDirectory: './builds/portable/VPalm_IDE-win32-x64',
  outputDirectory: './builds/installers',
  loadingGif: './www/install.gif',
  iconURL: 'cc.ico',
  setupIcon: 'cc.ico',
  version: '1.0.0',
  authors: "R. Vezy - R. Perez - J. Dauzat",
  owners: "CIRAD - UMR AMAP",
  title: "VPalm IDE"
};

console.log('Creating package (this may take a while)')

resultPromise = electronInstaller.createWindowsInstaller(settings);

resultPromise.then(() => {
    console.log("The installers of your application were succesfully created !");
}, (e) => {
    console.log(`There was an error building the windows installers for the app: ${e.message}`)
});
