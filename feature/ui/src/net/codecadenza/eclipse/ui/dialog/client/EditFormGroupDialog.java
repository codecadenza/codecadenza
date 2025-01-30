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

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import java.util.HashMap;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import org.eclipse.emf.ecore.xmi.XMLResource;
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
 * Dialog for creating and maintaining form groups
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditFormGroupDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit form group";
	private static final String DLG_TITLE_NEW = "Create new form group";

	private Text txtName;
	private final Project project;
	private FormGroup group;
	private FormGroup parentGroup;
	private boolean doEdit;
	private CheckboxDataGridComposite<Role> chkViewerRoles;
	private int order;
	private String title = DLG_TITLE_NEW;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param project
	 */
	public EditFormGroupDialog(Shell parentShell, Project project) {
		super(parentShell);

		this.project = project;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param parentGroup
	 * @param addToGroup
	 */
	public EditFormGroupDialog(Shell parentShell, FormGroup parentGroup, boolean addToGroup) {
		super(parentShell);

		this.doEdit = !addToGroup;
		this.parentGroup = parentGroup;
		this.project = parentGroup.findProject();

		if (doEdit)
			this.title = DLG_TITLE_EDIT;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param group
	 */
	public EditFormGroupDialog(Shell parentShell, FormGroup group) {
		super(parentShell);

		this.doEdit = true;
		this.group = group;
		this.title = DLG_TITLE_EDIT;
		this.project = group.findProject();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		if (doEdit)
			order = group.getGroupOrder();
		else {
			if (parentGroup == null) {
				order = -1;

				for (final FormGroup f : project.getFormGroups())
					if (f.getGroupOrder() > order)
						order = f.getGroupOrder();
			}
			else {
				order = -1;

				for (final FormGroup f : parentGroup.getChildGroups())
					if (f.getGroupOrder() > order)
						order = f.getGroupOrder();
			}

			order++;
		}

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (doEdit)
			txtName.setText(group.getName());

		final var lblRoles = new Label(panDialogArea, SWT.NONE);
		lblRoles.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		lblRoles.setText("Select roles:");

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
		chkViewerRoles.setData(project.getRoles());

		// Add the roles that should be selected
		if (doEdit)
			chkViewerRoles.setCheckedElements(group.getRoles());
		else if (parentGroup != null)
			chkViewerRoles.setCheckedElements(parentGroup.getRoles());

		return panDialogArea;
	}

	/**
	 * @return the form group
	 */
	public FormGroup getFormGroup() {
		return this.group;
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

			try {
				if (doEdit) {
					group.setName(txtName.getText());
					group.getRoles().clear();
				}
				else {
					group = ClientFactory.eINSTANCE.createFormGroup();
					group.setName(txtName.getText());
					group.setGroupOrder(order);

					if (parentGroup == null) {
						group.setProject(project);
						project.getFormGroups().add(group);
						project.eResource().getContents().add(group);
					}
					else {
						group.setParentGroup(parentGroup);
						parentGroup.getChildGroups().add(group);
					}
				}

				// Grant all selected roles
				group.getRoles().addAll(chkViewerRoles.getCheckedElements());

				if (parentGroup == null)
					EclipseIDEService.saveProjectMetaData(project);
				else {
					final var options = new HashMap<String, String>();
					options.put(XMLResource.OPTION_PROCESS_DANGLING_HREF, "DISCARD");

					parentGroup.eResource().save(options);
				}
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
