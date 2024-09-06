const vscode = require('vscode');
const positron = require('positron');
const pedant = require('./src/pedant.js');

function activate(context) {

	context.subscriptions.push(
		vscode.commands.registerCommand(
			'pedant.makeRFunctionCallExplicit', pedant.makeRFunctionCallExplicit
		),
		vscode.commands.registerCommand(
			'pedant.installPedant', pedant.installPedant
		)
	);
}
function deactivate() { }

module.exports = {
	activate,
	deactivate
};
