electron = require('electron');

app = electron.app; 
menu = electron.Menu;
BrowserWindow = electron.BrowserWindow;

var mainWindow = null;

var myMenu = menu.buildFromTemplate([
    {
        label: 'InfinityWar',
        click(){
            url = 'file://' + __dirname + '/frontend.html';
            mainWindow.loadURL(url);
        }
        
    },
    {   
        label: 'Endgame',
        click() {
            app.quit();
            app.exit(0);
        }
    }
]);

menu.setApplicationMenu(myMenu);

app.on('window-all-closed', function(){
    if(process.platform !== 'darwin') app.quit();
    app.quit();
    app.exit(0);
});

app.on('ready', function(){
    mainWindow = new BrowserWindow({width: 1600, height: 1400});
    url = 'file://' + __dirname + '/frontend.html';
    mainWindow.loadURL(url);
    mainWindow.on('closed', function(){
        mainWindow = app.exit(0);
    });
});