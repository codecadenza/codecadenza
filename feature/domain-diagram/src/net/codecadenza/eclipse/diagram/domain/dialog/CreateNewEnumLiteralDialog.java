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
package net.codecadenza.eclipse.diagram.domain.dialog;

import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating new enumeration literals
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateNewEnumLiteralDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "New enumeration literal";

	private Text txtName;
	private final EnumLiteral literal;
	private final JavaEnum javaEnum;
	private Combo cboTag;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param literal
	 * @param javaEnum
	 */
	public CreateNewEnumLiteralDialog(Shell parentShell, EnumLiteral literal, JavaEnum javaEnum) {
		super(parentShell);

		this.literal = literal;
		this.javaEnum = javaEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		final var gdName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdName.widthHint = 300;

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setText("LITERAL");
		txtName.setLayoutData(gdName);
		txtName.selectAll();
		txtName.setFocus();

		final var lblTag = new Label(panDialogArea, SWT.NONE);
		lblTag.setText("Tag:");

		cboTag = new Combo(panDialogArea, SWT.READ_ONLY);
		cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		// Add valid enumeration tags!
		javaEnum.getValidLiteralsTagsOfEnum().forEach(cboTag::add);

		cboTag.select(0);

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			final IStatus status = EclipseIDEService.validateFieldName(txtName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				return;
			}

			for (final EnumLiteral existingLiteral : javaEnum.getEnumerationValues())
				if (existingLiteral.getName() != null && existingLiteral.getName().equals(txtName.getText())) {
					MessageDialog.openInformation(getShell(), DLG_TITLE, "An enumeration literal with the same name already exists!");
					return;
				}

			literal.setName(txtName.getText());
			literal.setTag(EnumLiteralTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex())));
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

		newShell.setText(DLG_TITLE);
	}

}
