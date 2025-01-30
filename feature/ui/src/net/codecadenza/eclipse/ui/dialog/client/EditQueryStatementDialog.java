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
package net.codecadenza.eclipse.ui.dialog.client;

import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining query statements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditQueryStatementDialog extends CodeCadenzaDialog {
	private Text txtGenStatement;
	private Text txtCustomStatement;
	private final BoundaryMethod fetchMethod;
	private BoundaryMethod countMethod;

	/**
	 * Constructor
	 * @param parentShell
	 * @param fetchMethod
	 */
	public EditQueryStatementDialog(Shell parentShell, BoundaryMethod fetchMethod) {
		super(parentShell);

		this.fetchMethod = fetchMethod;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param fetchMethod
	 * @param countMethod
	 */
	public EditQueryStatementDialog(Shell parentShell, BoundaryMethod fetchMethod, BoundaryMethod countMethod) {
		this(parentShell, fetchMethod);

		this.countMethod = countMethod;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var lblGenStatement = new Label(panDialogArea, SWT.NONE);
		lblGenStatement.setText("Generated statement:");

		final var gdGenStatement = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdGenStatement.heightHint = 200;
		gdGenStatement.widthHint = 500;

		txtGenStatement = new Text(panDialogArea, SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.CANCEL | SWT.MULTI);
		txtGenStatement.setLayoutData(gdGenStatement);
		txtGenStatement.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		final var lblCustomStatement = new Label(panDialogArea, SWT.NONE);
		lblCustomStatement.setText("Additional statement:");

		final var gdCustomStatement = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdCustomStatement.heightHint = 200;

		txtCustomStatement = new Text(panDialogArea, SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.CANCEL | SWT.MULTI);
		txtCustomStatement.setLayoutData(gdCustomStatement);
		txtCustomStatement.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

		txtCustomStatement.setText(fetchMethod.getCustomStatement());
		txtGenStatement.setText(fetchMethod.getQueryStatement());

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			fetchMethod.setQueryStatement(txtGenStatement.getText());
			fetchMethod.setCustomStatement(txtCustomStatement.getText());

			if (countMethod != null) {
				countMethod.setQueryStatement(txtGenStatement.getText());
				countMethod.setCustomStatement(txtCustomStatement.getText());
			}
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Edit query statement");
	}

}
