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
package net.codecadenza.eclipse.ui.dialog.project;

import net.codecadenza.eclipse.model.project.PersistenceUnitProperty;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining persistence unit properties
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditPersistenceUnitPropertyDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit persistence unit property";
	private static final String DLG_TITLE_CREATE = "Create new persistence unit property";

	private final Project project;
	private PersistenceUnitProperty property;
	private Text txtValue;
	private Text txtName;
	private String title = DLG_TITLE_CREATE;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 * @param property
	 */
	public EditPersistenceUnitPropertyDialog(Shell parentShell, Project project, PersistenceUnitProperty property) {
		super(parentShell);

		this.project = project;
		this.property = property;

		if (property != null)
			title = DLG_TITLE_EDIT;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Property name:");

		final var gdName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdName.widthHint = 300;

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(gdName);

		final var lblValue = new Label(panDialogArea, SWT.NONE);
		lblValue.setText("Property value:");

		txtValue = new Text(panDialogArea, SWT.BORDER);
		txtValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (property != null) {
			txtName.setText(property.getName());
			txtValue.setText(property.getValue());
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (txtName.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The name must not be empty!");
				txtName.setFocus();
				return;
			}

			if (txtValue.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The value must not be empty!");
				txtValue.setFocus();
				return;
			}

			// Check if a persistence unit property with the same name already exists
			for (final PersistenceUnitProperty prop : project.getPersistenceUnitProperties())
				if (!prop.equals(property) && prop.getName().equals(txtName.getText())) {
					MessageDialog.openInformation(getShell(), title, "A persistence unit property with the same name already exists!");
					return;
				}

			if (property == null)
				property = ProjectFactory.eINSTANCE.createPersistenceUnitProperty();

			property.setName(txtName.getText());
			property.setValue(txtValue.getText());
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

		newShell.setText(title);
	}

	/**
	 * @return the persistence unit property
	 */
	public PersistenceUnitProperty getProperty() {
		return property;
	}

}
