// Handling Squirrel Events (see https://github.com/electron/windows-installer)
if (require('electron-squirrel-startup')) return;

// Now this is the code to build the electron app:
const electron = require('electron');
// Module to control application life.
const app = electron.app;

if (handleSquirrelEvent()) {
    // squirrel event handled and app will exit in 1000ms, so don't do anything else
    return;
}

function handleSquirrelEvent() {
    if (process.argv.length === 1) {
        return false;
    }

    const ChildProcess = require('child_process');
    const path = require('path');

    const appFolder = path.resolve(process.execPath, '..');
    const rootAtomFolder = path.resolve(appFolder, '..');
    const updateDotExe = path.resolve(path.join(rootAtomFolder, 'Update.exe'));
    const exeName = path.basename(process.execPath);

    const spawn = function (command, args) {
        let spawnedProcess, error;

        try {
            spawnedProcess = ChildProcess.spawn(command, args, { detached: true });
        } catch (error) { }

        return spawnedProcess;
    };

    const spawnUpdate = function (args) {
        return spawn(updateDotExe, args);
    };

    const squirrelEvent = process.argv[1];
    switch (squirrelEvent) {
        case '--squirrel-install':
        case '--squirrel-updated':
            // Optionally do things such as:
            // - Add your .exe to the PATH
            // - Write to the registry for things like file associations and
            //   explorer context menus

            // Install desktop and start menu shortcuts
            spawnUpdate(['--createShortcut', exeName]);

            setTimeout(app.quit, 1000);
            return true;

        case '--squirrel-uninstall':
            // Undo anything you did in the --squirrel-install and
            // --squirrel-updated handlers

            // Remove desktop and start menu shortcuts
            spawnUpdate(['--removeShortcut', exeName]);

            setTimeout(app.quit, 1000);
            return true;

        case '--squirrel-obsolete':
            // This is called on the outgoing version of your app before
            // we update to the new version - it's the opposite of
            // --squirrel-updated

            app.quit();
            return true;
    }
};


// Module to create native browser window.
const BrowserWindow = electron.BrowserWindow

const path = require('path')
const url = require('url')
const port = "9191"
const child = require('child_process');
//const cmdStr = ".\\R-Portable\\bin\\Rscript.exe -e \"shiny::runApp('shinyApp.R', port="+port+")\"";
//ShinyProjects\MarketSizing\Market_sizing_Cardinal-master-b823f3aa918475f5d56f01aec9763ed860715158\mrktsiz_no_crosstalk
const killStr = "taskkill /im Rscript.exe /f"

var execPath = "Rscript"
// if(process.patform == "win32"){
execPath = path.join(app.getAppPath(), "R-Portable", "bin", "Rscript.exe")
// }
var appPath = app.getAppPath()
appPath = appPath.replace(/\\/g, "\\\\");
const childProcess = child.spawn(execPath, ["-e", "shiny::runApp(file.path('" + appPath + "'), port=" + port + ")"])
childProcess.stdout.on('data', (data) => {
    console.log(`stdout:${data}`)
})
childProcess.stderr.on('data', (data) => {
    console.log(`stdout:${data}`)
})

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let mainWindow

function createWindow() {
    // Create the browser window.
    //  mainWindow = new BrowserWindow({webPreferences:{nodeIntegration:false},width: 800, height: 600})
    //  console.log(process.cwd())
    console.log('create-window')


    let loading = new BrowserWindow({ show: false, frame: false })
    //let loading = new BrowserWindow()
    console.log(new Date().toISOString() + '::showing loading');
    loading.loadURL("data:text/html;charset=utf-8;base64,PGh0bWw+DQo8c3R5bGU+DQpib2R5ew0KICBwYWRkaW5nOiAxZW07DQogIGNvbG9yOiAjNzc3Ow0KICB0ZXh0LWFsaWduOiBjZW50ZXI7DQogIGZvbnQtZmFtaWx5OiAiR2lsbCBzYW5zIiwgc2Fucy1zZXJpZjsNCiAgd2lkdGg6IDgwJTsNCiAgbWFyZ2luOiAwIGF1dG87DQp9DQpoMXsNCiAgbWFyZ2luOiAxZW0gMDsNCiAgYm9yZGVyLWJvdHRvbTogMXB4IGRhc2hlZDsNCiAgcGFkZGluZy1ib3R0b206IDFlbTsNCiAgZm9udC13ZWlnaHQ6IGxpZ2h0ZXI7DQp9DQpwew0KICBmb250LXN0eWxlOiBpdGFsaWM7DQp9DQoubG9hZGVyew0KICBtYXJnaW46IDAgMCAyZW07DQogIGhlaWdodDogMTAwcHg7DQogIHdpZHRoOiAyMCU7DQogIHRleHQtYWxpZ246IGNlbnRlcjsNCiAgcGFkZGluZzogMWVtOw0KICBtYXJnaW46IDAgYXV0byAxZW07DQogIGRpc3BsYXk6IGlubGluZS1ibG9jazsNCiAgdmVydGljYWwtYWxpZ246IHRvcDsNCn0NCg0KLyoNCiAgU2V0IHRoZSBjb2xvciBvZiB0aGUgaWNvbg0KKi8NCnN2ZyBwYXRoLA0Kc3ZnIHJlY3R7DQogIGZpbGw6ICNGRjY3MDA7DQp9DQo8L3N0eWxlPg0KPGJvZHk+PCEtLSAzICAtLT4NCjxkaXYgY2xhc3M9ImxvYWRlciBsb2FkZXItLXN0eWxlMyIgdGl0bGU9IjIiPg0KICA8c3ZnIHZlcnNpb249IjEuMSIgaWQ9ImxvYWRlci0xIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB4PSIwcHgiIHk9IjBweCINCiAgICAgd2lkdGg9IjgwcHgiIGhlaWdodD0iODBweCIgdmlld0JveD0iMCAwIDUwIDUwIiBzdHlsZT0iZW5hYmxlLWJhY2tncm91bmQ6bmV3IDAgMCA1MCA1MDsiIHhtbDpzcGFjZT0icHJlc2VydmUiPg0KICA8cGF0aCBmaWxsPSIjMDAwIiBkPSJNNDMuOTM1LDI1LjE0NWMwLTEwLjMxOC04LjM2NC0xOC42ODMtMTguNjgzLTE4LjY4M2MtMTAuMzE4LDAtMTguNjgzLDguMzY1LTE4LjY4MywxOC42ODNoNC4wNjhjMC04LjA3MSw2LjU0My0xNC42MTUsMTQuNjE1LTE0LjYxNWM4LjA3MiwwLDE0LjYxNSw2LjU0MywxNC42MTUsMTQuNjE1SDQzLjkzNXoiPg0KICAgIDxhbmltYXRlVHJhbnNmb3JtIGF0dHJpYnV0ZVR5cGU9InhtbCINCiAgICAgIGF0dHJpYnV0ZU5hbWU9InRyYW5zZm9ybSINCiAgICAgIHR5cGU9InJvdGF0ZSINCiAgICAgIGZyb209IjAgMjUgMjUiDQogICAgICB0bz0iMzYwIDI1IDI1Ig0KICAgICAgZHVyPSIwLjZzIg0KICAgICAgcmVwZWF0Q291bnQ9ImluZGVmaW5pdGUiLz4NCiAgICA8L3BhdGg+DQogIDwvc3ZnPg0KPC9kaXY+DQo8L2JvZHk+DQo8L2h0bWw+");
    //loading.loadURL("http://www.google.com")
    ///loading.toggleDevTools()

    loading.once('show', () => {
        console.log(new Date().toISOString() + '::show loading')
        mainWindow = new BrowserWindow({ webPreferences: { nodeIntegration: false }, show: false, width: 800, height: 600, title: "" })

        mainWindow.webContents.once('dom-ready', () => {
            console.log(new Date().toISOString() + '::mainWindow loaded')
            mainWindow.show()
            loading.hide()
            loading.close()
        })
        console.log(port)
        // long loading html
        mainWindow.loadURL('http://127.0.0.1:' + port)

        /**
        mainWindow.loadURL(url.format({
          pathname: path.join(__dirname, 'index.html'),
          protocol: 'file:',
          slashes: true
        }))
        **/

        mainWindow.webContents.on('did-finish-load', function () {
            console.log(new Date().toISOString() + '::did-finish-load')
        });

        mainWindow.webContents.on('did-start-load', function () {
            console.log(new Date().toISOString() + '::did-start-load')
        });

        mainWindow.webContents.on('did-stop-load', function () {
            console.log(new Date().toISOString() + '::did-stop-load')
        });
        mainWindow.webContents.on('dom-ready', function () {
            console.log(new Date().toISOString() + '::dom-ready')
        });

        // Open the DevTools.
        // mainWindow.webContents.openDevTools()

        // Emitted when the window is closed.
        mainWindow.on('closed', function () {
            console.log(new Date().toISOString() + '::closed')
            cleanUpApplication()
        })
    })

    loading.show()

}

function cleanUpApplication() {
    if (childProcess) {
        console.log('window-all-closed   ... going to kill')
        try {
            childProcess.kill();
            console.log('killed.')
            child.execSync(killStr)
        } catch (ex) {
            console.log(ex)
        }
    }

    mainWindow = null

}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createWindow)

// Quit when all windows are closed.
app.on('window-all-closed', function () {
    console.log('EVENT::window-all-closed')
    // On OS X it is common for applications and their menu bar
    // to stay active until the user quits explicitly with Cmd + Q
    if (process.platform !== 'darwin') {
        app.quit()
    }

    cleanUpApplication()

})

app.on('activate', function () {
    // On OS X it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (mainWindow === null) {
        createWindow()
    }
})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.
