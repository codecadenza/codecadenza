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

import java.util.HashMap;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectFactory;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining roles
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditRoleDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit role";
	private static final String DLG_TITLE_NEW = "Create new role";

	private final Project project;
	private final HashMap<String, Role> roles = new HashMap<>();
	private final boolean doEdit;
	private Text txtName;
	private Button chkAdmin;
	private Button chkReadonly;
	private Role role;
	private String oldName = "";
	private String title = DLG_TITLE_NEW;

	/**
	 * Constructor
	 * @param parentShell
	 * @param project
	 * @param role
	 */
	public EditRoleDialog(Shell parentShell, Project project, Role role) {
		super(parentShell);

		this.doEdit = true;
		this.role = role;
		this.project = project;
		this.title = DLG_TITLE_EDIT;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param project
	 */
	public EditRoleDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.doEdit = false;
		this.project = project;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		project.getRoles().forEach(r -> roles.put(r.getName(), r));

		final var panDialogArea = (Composite) super.createDialogArea(parent, 2, false);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		final var gdName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdName.widthHint = 300;

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(gdName);

		final var lblAdmin = new Label(panDialogArea, SWT.NONE);
		lblAdmin.setText("Administrator:");

		chkAdmin = new Button(panDialogArea, SWT.CHECK);

		final var lblReadonly = new Label(panDialogArea, SWT.NONE);
		lblReadonly.setText("Readonly:");

		chkReadonly = new Button(panDialogArea, SWT.CHECK);

		if (doEdit) {
			txtName.setText(role.getName());
			oldName = role.getName();
			chkAdmin.setSelection(role.isAdminRole());
			chkReadonly.setSelection(role.isReadonlyRole());
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
			// Check if the user has inserted a name
			if (txtName.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The name must not be empty!");
				txtName.setFocus();
				return;
			}

			if (!oldName.equals(txtName.getText()) && roles.containsKey(txtName.getText())) {
				MessageDialog.openInformation(getShell(), title, "A role with the same name already exists!");
				txtName.setFocus();
				return;
			}

			// The name of the role must follow Java naming conventions!
			final IStatus status = EclipseIDEService.validateTypeVariableName(txtName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), title, status.getMessage());
				return;
			}

			if (!doEdit)
				role = ProjectFactory.eINSTANCE.createRole();

			role.setName(txtName.getText());
			role.setAdminRole(chkAdmin.getSelection());
			role.setReadonlyRole(chkReadonly.getSelection());

			if (!doEdit)
				project.getRoles().add(role);

			try {
				EclipseIDEService.saveProjectMetaData(project);
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
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

		newShell.setText(title);
	}

}
