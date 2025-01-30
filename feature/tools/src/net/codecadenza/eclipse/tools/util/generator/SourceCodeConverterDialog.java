/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.tools.util.generator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Internal dialog for converting a source code fragment into a format that can be directly used in a generator
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SourceCodeConverterDialog extends Dialog {
	private Shell shell;
	private Text txtOutput;
	private Text txtInput;

	/**
	 * Create the dialog
	 * @param parent
	 * @param style
	 */
	public SourceCodeConverterDialog(Shell parent, int style) {
		super(parent, style);
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		final var display = new Display();
		final var shell = new Shell(display);

		// Create and open the dialog
		new SourceCodeConverterDialog(shell, SWT.NONE).open();
	}

	/**
	 * Open the dialog
	 */
	private void open() {
		shell = new Shell(getParent(), SWT.DIALOG_TRIM | SWT.RESIZE);
		shell.setSize(600, 600);
		shell.setText("Source code converter");
		shell.setLayout(new FillLayout(SWT.HORIZONTAL));

		createContents();

		shell.open();
		shell.layout();

		final Display display = getParent().getDisplay();

		while (!shell.isDisposed())
			if (!display.readAndDispatch())
				display.sleep();

		display.dispose();
	}

	/**
	 * Create the contents of the dialog
	 */
	private void createContents() {
		final var sashForm = new SashForm(shell, SWT.BORDER | SWT.VERTICAL);

		final var panInput = new Composite(sashForm, SWT.NONE);
		panInput.setLayout(new GridLayout());

		final var lblInput = new Label(panInput, SWT.NONE);
		lblInput.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		lblInput.setText("Enter source code to be converted...");

		txtInput = new Text(panInput, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		txtInput.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		txtInput.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				convert();
			}
		});

		final var panOutput = new Composite(sashForm, SWT.NONE);
		panOutput.setLayout(new GridLayout());

		final var lblOutput = new Label(panOutput, SWT.NONE);
		lblOutput.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		lblOutput.setText("Conversion result:");

		txtOutput = new Text(panOutput, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		txtOutput.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		sashForm.setWeights(1, 1);
	}

	/**
	 * Convert the source code fragment in the input text field and display the result in the output text field
	 */
	private void convert() {
		final var b = new StringBuilder();
		b.append("StringBuilder b = new StringBuilder();\n\n");

		final String[] lines = txtInput.getText().split(System.lineSeparator());

		// Convert every single line (e.g. remove tabs)
		for (final String line : lines)
			b.append("b.append(\"" + line.replace("\"", "\\\"").replace("\t", "") + "\\n\");\n");

		b.append("\n");
		b.append("return b.toString();\n");

		txtOutput.setText(b.toString());
	}

}
