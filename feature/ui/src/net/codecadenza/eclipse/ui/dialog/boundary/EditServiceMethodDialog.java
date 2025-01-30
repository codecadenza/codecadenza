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
package net.codecadenza.eclipse.ui.dialog.boundary;

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import java.util.Collection;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.TransactionTypeEnumeration;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;

/**
 * <p>
 * Dialog for maintaining service methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditServiceMethodDialog extends CodeCadenzaDialog {
	private final ServiceMethod method;
	private final Collection<Role> roles;
	private Combo cboPermMode;
	private Table table;
	private Combo cboTransType;
	private CheckboxDataGridComposite<Role> chkViewerRoles;
	private Label lblRoles;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param method the method to edit
	 * @param project
	 */
	public EditServiceMethodDialog(Shell parentShell, ServiceMethod method, Project project) {
		super(parentShell);

		this.method = method;
		this.roles = project.getRoles();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		int index = 0;
		boolean canEditRoles = false;

		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblTransType = new Label(panDialogArea, SWT.NONE);
		lblTransType.setText("Transaction type:");

		cboTransType = new Combo(panDialogArea, SWT.READ_ONLY);

		// Add transaction types
		for (final TransactionTypeEnumeration transType : TransactionTypeEnumeration.values()) {
			cboTransType.add(transType.toString());

			if (transType == method.getTransactionType())
				cboTransType.select(index);

			index++;
		}

		cboTransType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblPermissionMode = new Label(panDialogArea, SWT.NONE);
		lblPermissionMode.setText("Permission mode:");

		cboPermMode = new Combo(panDialogArea, SWT.READ_ONLY);

		cboPermMode.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final String mode = cboPermMode.getItem(cboPermMode.getSelectionIndex());

				if (PermissionModeEnumeration.valueOf(mode) == PermissionModeEnumeration.DEDICATED_ROLES) {
					lblRoles.setEnabled(true);
					table.setEnabled(true);
				}
				else {
					lblRoles.setEnabled(false);
					table.setEnabled(false);
					chkViewerRoles.uncheckAllElements();
				}
			}
		});

		cboPermMode.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		index = 0;

		// Add permission modes
		for (final PermissionModeEnumeration permMode : PermissionModeEnumeration.values()) {
			cboPermMode.add(permMode.toString());

			if (permMode == method.getPermissionMode())
				cboPermMode.select(index);

			index++;
		}

		if (method.getPermissionMode() == PermissionModeEnumeration.DEDICATED_ROLES)
			canEditRoles = true;

		lblRoles = new Label(panDialogArea, SWT.NONE);
		lblRoles.setEnabled(canEditRoles);
		lblRoles.setText("Select roles:");

		new Label(panDialogArea, SWT.NONE);

		chkViewerRoles = new CheckboxDataGridComposite<>(panDialogArea, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(Role element, int columnIndex) {
				if (columnIndex == 0)
					return element.getName();

				return "";
			}
		};

		chkViewerRoles.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
		chkViewerRoles.addColumn("Name", ColumnSortType.STRING, 400);
		chkViewerRoles.getTableViewer().getTable().setLinesVisible(false);
		chkViewerRoles.setData(roles);
		chkViewerRoles.setCheckedElements(method.getRoles());

		table = chkViewerRoles.getTableViewer().getTable();
		table.setEnabled(canEditRoles);

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Edit method '" + method.getName() + "'");
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Set method data
			method.setPermissionMode(PermissionModeEnumeration.valueOf(cboPermMode.getItem(cboPermMode.getSelectionIndex())));
			method.setTransactionType(TransactionTypeEnumeration.valueOf(cboTransType.getItem(cboTransType.getSelectionIndex())));
			method.getRoles().clear();

			if (PermissionModeEnumeration
					.valueOf(cboPermMode.getItem(cboPermMode.getSelectionIndex())) == PermissionModeEnumeration.DEDICATED_ROLES) {
				// Grant all selected roles to this method
				method.getRoles().addAll(chkViewerRoles.getCheckedElements());
			}
		}

		super.buttonPressed(buttonId);
	}

}
