const vscode = require('vscode');
const positron = require('positron');
const fs = require('fs');
const os = require('os');
const path = require('path');

async function makeRFunctionCallExplicit() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showErrorMessage("No active editor found!");
        return;
    }

    const selection = editor.selection;
    const text = editor.document.getText(selection);

    if (!text) {
        vscode.window.showErrorMessage("No text selected!");
        return;
    }

    // Create a temporary file path
    const tempFilePath = path.join(os.tmpdir(), `pedant_output_${Date.now()}.txt`);
    const normalizedPath = tempFilePath.replace(/\\/g, "/");

    // Write the text to the temporary file
    await fs.writeFile(tempFilePath, text, 'utf8', (err) => {
        if (err) {
            vscode.window.showErrorMessage(`Failed to write to temp file: ${err.message}`);
            return;
        }
    });

    try {
        // const rCommand = `tryCatch(writeLines(pedant::add_double_colons('${text}'), '${normalizedPath}'), error = function(e) writeLines('Error_001', '${normalizedPath}'))`;
        const rCommand = `pedant:::ic('${normalizedPath}')`;

        // Send the R command to the Positron Console
        await positron.runtime.executeCode('r', rCommand, false, false);

        // Wait for the command to complete
        setTimeout(async () => {
            // Read the output from the temporary file
            await fs.readFile(tempFilePath, 'utf8', (err, output) => {
                if (err) {
                    vscode.window.showErrorMessage(`Failed to read output file: ${err.message}`);
                    return;
                }

                if (!output.includes("Error_00")) {
                    editor.edit(editBuilder => {
                        editBuilder.replace(selection, output);
                    });
                } else if (output.includes("Error_002")) {
                    vscode.window.showErrorMessage(`Failed to write selection: ${err.message}`);
                } else {
                    vscode.window.showErrorMessage("{pedant} package is not installed or outdated. Run `pedant.installPedant` to install/update it.");
                }

                // Delete the temporary file after reading
                fs.unlink(tempFilePath, (err) => {
                    if (err) {
                        console.error(`Failed to delete temp file: ${err.message}`);
                    }
                });
            });
        }, 100); // Adjust the timeout based on the expected execution time

    } catch (error) {
        vscode.window.showErrorMessage(`Fail: ${error.message}`);
    }
}

async function installPedant() {
    try {
        // const rCommand = `if (!requireNamespace("pedant", quietly = TRUE)) pak::pak('wurli/pedant')`;
        const rCommand = `if (!(requireNamespace("pedant", quietly = TRUE) && packageVersion("pedant") >= "0.1.1")) pak::pak("atsyplenkov/pedant@positron-ext")`;

        // Send the R command to the Positron Console
        await positron.runtime.executeCode('r', rCommand, false, true);


    } catch (error) {
        vscode.window.showErrorMessage(`Failed to check or install pedant package: ${error.message}`);
    }
}

module.exports = {
    makeRFunctionCallExplicit,
    installPedant
}