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

import net.codecadenza.eclipse.model.java.EnumTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
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
 * Dialog for creating new enumerations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateNewJavaEnumDialog extends CodeCadenzaTitleAreaDialog {
	private static final String DLG_TITLE = "Create new enumeration";

	private Text txtComment;
	private Text txtName;
	private final JavaEnum enumeration;
	private Combo cboTag;

	/**
	 * Constructor to initialize and create the dialog
	 * @param parentShell
	 * @param enumeration
	 */
	public CreateNewJavaEnumDialog(Shell parentShell, JavaEnum enumeration) {
		super(parentShell);

		this.enumeration = enumeration;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setText("Enumeration");
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtName.selectAll();
		txtName.setFocus();

		final var lblTag = new Label(panDialogArea, SWT.NONE);
		lblTag.setText("Tag:");

		cboTag = new Combo(panDialogArea, SWT.READ_ONLY);
		cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		// Add all valid enumeration tags
		enumeration.getNamespace().getProject().getValidEnumTags().forEach(cboTag::add);

		cboTag.select(0);

		final var lblComment = new Label(panDialogArea, SWT.NONE);
		lblComment.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
		lblComment.setText("Comment:");

		txtComment = new Text(panDialogArea, SWT.MULTI | SWT.BORDER);
		txtComment.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		setTitle("New enumeration");
		setMessage("Enter data...");

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the name
			final IStatus status = EclipseIDEService.validateJavaTypeName(txtName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				setErrorMessage(status.getMessage());
				return;
			}

			enumeration.setName(txtName.getText());
			enumeration.setMappable(true);
			enumeration.setPrimitive(false);
			enumeration.setComment(txtComment.getText());
			enumeration.setTag(EnumTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex())));
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
