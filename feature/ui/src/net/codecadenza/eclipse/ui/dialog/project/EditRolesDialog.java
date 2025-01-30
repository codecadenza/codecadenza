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

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for maintaining project roles
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditRolesDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit roles";

	private final Project project;
	private DataGridComposite<Role> gridRoles;

	/**
	 * Constructor
	 * @param parentShell
	 * @param project
	 */
	public EditRolesDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
	}

	/**
	 * Edit the selected role
	 */
	private void editRole() {
		final Role role = gridRoles.getSelection();

		if (role == null)
			return;

		final var dlg = new EditRoleDialog(getShell(), project, role);

		if (dlg.open() == Dialog.OK)
			gridRoles.refresh();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final GridData gdRoles = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdRoles.heightHint = 400;

		gridRoles = new DataGridComposite<>(panDialogArea, SWT.BORDER | SWT.FULL_SELECTION, REMOVE_DEFAULT_MENU_ITEMS) {
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

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#onDoubleClick(java.lang.Object)
			 */
			@Override
			public void onDoubleClick(Role element) {
				editRole();
			}
		};

		gridRoles.setLayoutData(gdRoles);
		gridRoles.addColumn("Name", ColumnSortType.STRING, 400);
		gridRoles.setData(project.getRoles());

		final var panButtons = new Composite(panDialogArea, SWT.NONE);
		panButtons.setLayoutData(new GridData(SWT.CENTER, SWT.TOP, false, false));
		panButtons.setLayout(new GridLayout());

		final var cmdAddRole = new Button(panButtons, SWT.NONE);
		cmdAddRole.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		cmdAddRole.setText("Add");

		cmdAddRole.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlg = new EditRoleDialog(getShell(), project);
				dlg.open();

				if (dlg.getReturnCode() == Dialog.OK)
					gridRoles.refresh();
			}
		});

		final var cmdEdit = new Button(panButtons, SWT.NONE);
		cmdEdit.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		cmdEdit.setText("Edit");

		cmdEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editRole();
			}
		});

		final var cmdRemoveRole = new Button(panButtons, SWT.NONE);
		cmdRemoveRole.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		cmdRemoveRole.setText("Remove");

		cmdRemoveRole.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final Role role = gridRoles.getSelection();

				if (role == null)
					return;

				project.getRoles().remove(role);

				try {
					EclipseIDEService.saveProjectMetaData(project);
					gridRoles.refresh();
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, true);
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
