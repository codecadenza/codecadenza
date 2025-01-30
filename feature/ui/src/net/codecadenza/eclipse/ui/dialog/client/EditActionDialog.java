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

import java.util.Collection;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.service.ServiceMethod;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
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
 * Dialog for creating and maintaining form actions
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditActionDialog extends CodeCadenzaDialog {
	public static final int DELETE = 100;
	private static final String DLG_TITLE_EDIT = "Edit form action";
	private static final String DLG_TITLE_NEW = "Create new form action";

	private final Collection<Role> roles;
	private final EList<DataExchangeMethod> methods = new BasicEList<>();
	private FormAction action;
	private CheckboxDataGridComposite<Role> chkViewerRoles;
	private Text txtName;
	private Text txtDescription;
	private boolean editMode = true;
	private Text txtRefObject;
	private DataComboViewer<DataExchangeMethod> cboExchangeMethod;
	private Form form;
	private FormPanel formPanel;
	private String title = DLG_TITLE_NEW;
	private Project project;
	private DomainObject domainObject;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param roles
	 * @param action
	 */
	public EditActionDialog(Shell parentShell, Collection<Role> roles, FormAction action) {
		super(parentShell);

		this.roles = roles;
		this.action = action;
		this.title = DLG_TITLE_EDIT;
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param form
	 * @param roles
	 */
	public EditActionDialog(Shell parentShell, Form form, Collection<Role> roles) {
		super(parentShell);

		this.roles = roles;
		this.editMode = false;
		this.form = form;
		this.domainObject = form.getDomainObject();
		this.project = domainObject.getNamespace().getProject();
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param formPanel
	 * @param roles
	 */
	public EditActionDialog(Shell parentShell, FormPanel formPanel, Collection<Role> roles) {
		super(parentShell);

		this.roles = roles;
		this.editMode = false;
		this.formPanel = formPanel;
		this.domainObject = formPanel.getDTO().getDomainObject();
		this.project = domainObject.getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Action field name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblRefObject = new Label(panDialogArea, SWT.NONE);

		if (editMode) {
			if (action.getBoundaryMethod() != null)
				lblRefObject.setText("Method:");
			else if (action.getTargetForm() != null)
				lblRefObject.setText("Target form:");

			txtRefObject = new Text(panDialogArea, SWT.BORDER | SWT.READ_ONLY);
			txtRefObject.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			// Hide the text field if no reasonable output is available
			if (action.getBoundaryMethod() == null && action.getTargetForm() == null)
				txtRefObject.setVisible(false);
		}
		else {
			lblRefObject.setText("Exchange method:");

			cboExchangeMethod = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
				 */
				@Override
				public String getItemText(DataExchangeMethod element) {
					return element.getName();
				}

				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
				 */
				@Override
				public void onSelectionChanged(DataExchangeMethod element) {
					final String methodName = element.getName();
					String actionName = methodName;

					var description = "Perform " + element.getDataExchangeServiceBean().getDomainObject().getLabel();
					description += " " + element.getContentType().getName();
					description += " " + element.getMethodType().getName().toLowerCase();

					if (methodName.startsWith("perform"))
						actionName = methodName.replace("perform", "action");

					txtName.setText(actionName);
					txtDescription.setText(description);

					chkViewerRoles.uncheckAllElements();

					if (element.getPermissionMode() == PermissionModeEnumeration.DEDICATED_ROLES)
						chkViewerRoles.setCheckedElements(element.getRoles());
					else if (element.getPermissionMode() == PermissionModeEnumeration.PERMIT_ALL)
						chkViewerRoles.checkAllElements();
				}
			};

			cboExchangeMethod.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			for (final DataExchangeServiceBean exchangeService : project.getAllExchangeServicesOfDomainObject(domainObject)) {
				if (formPanel != null)
					for (final DataExchangeMethod exchangeMethod : exchangeService.getDataExchangeMethods()) {
						// A 'DIRECT' data exchange method must not be used by a form action!
						if (exchangeMethod.getExchangeMode() instanceof DirectExchangeMode)
							continue;

						// We don't add data import methods if the grid panel is read-only!
						if ((formPanel.getAssociation() instanceof ManyToManyAssociation
								|| formPanel.getAssociation() instanceof final OneToManyAssociation otm && !otm.isBidirectional())
								&& exchangeMethod.getMethodType() == DataExchangeMethodTypeEnumeration.IMPORT)
							continue;

						addMethodToList(exchangeMethod, formPanel.getActions());
					}

				if (form != null)
					for (final DataExchangeMethod exchangeMethod : exchangeService.getDataExchangeMethods()) {
						// A 'DIRECT' data exchange method must not be used by a form action!
						if (exchangeMethod.getExchangeMode() instanceof DirectExchangeMode)
							continue;

						addMethodToList(exchangeMethod, form.getActions());
					}
			}

			cboExchangeMethod.setData(methods);
		}

		final var lblDescription = new Label(panDialogArea, SWT.NONE);
		lblDescription.setText("Description:");

		txtDescription = new Text(panDialogArea, SWT.BORDER);
		txtDescription.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblRoles = new Label(panDialogArea, SWT.NONE);
		lblRoles.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
		lblRoles.setText("Roles:");

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

		chkViewerRoles.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		chkViewerRoles.addColumn("Name", ColumnSortType.STRING, 400);
		chkViewerRoles.getTableViewer().getTable().setLinesVisible(false);
		chkViewerRoles.setData(roles);

		if (editMode) {
			chkViewerRoles.setCheckedElements(action.getRoles());
			txtName.setText(action.getName());
			txtDescription.setText(action.getDescription());

			if (action.getBoundaryMethod() != null)
				txtRefObject.setText(action.getBoundaryMethod().getName());
			else if (action.getTargetForm() != null)
				txtRefObject.setText(action.getTargetForm().getName());
		}
		else if (!methods.isEmpty())
			cboExchangeMethod.setSelectedItem(methods.get(0));

		return panDialogArea;
	}

	/**
	 * @param exchangeMethod
	 * @param actions
	 */
	private void addMethodToList(DataExchangeMethod exchangeMethod, EList<FormAction> actions) {
		// Avoid creating an action that references the same method twice!
		for (final FormAction formAction : actions) {
			if (formAction.getBoundaryMethod() == null)
				continue;

			final ServiceMethod serviceMethod = formAction.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.equals(serviceMethod))
				return;
		}

		methods.add(exchangeMethod);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);

		if (editMode)
			createButton(parent, IDialogConstants.INTERNAL_ID, "Delete", false);
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

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the user input
			final IStatus status = EclipseIDEService.validateFieldName(txtName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), title, status.getMessage());
				txtName.setFocus();
				return;
			}

			if (!editMode && cboExchangeMethod.getSelectedItem() == null) {
				MessageDialog.openInformation(getShell(), title, "A data exchange method must be selected!");
				cboExchangeMethod.setFocus();
				return;
			}

			if (txtDescription.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The description must not be empty!");
				txtDescription.setFocus();
				return;
			}

			if (!editMode) {
				final DataExchangeMethod exchangeMethod = cboExchangeMethod.getSelectedItem();

				try {
					final BoundaryMethod boundaryMethod = new BoundaryService(project).initializeBoundaryMethod(exchangeMethod);

					action = ClientFactory.eINSTANCE.createFormAction();
					action.setBoundaryMethod(boundaryMethod);

					if (exchangeMethod.getMethodType() == DataExchangeMethodTypeEnumeration.EXPORT)
						action.setType(ActionType.DOWNLOAD_EXPORT);
					else
						action.setType(ActionType.UPLOAD_IMPORT);

					if (form != null) {
						action.setForm(form);
						form.getActions().add(action);
					}
					else {
						action.setPanel(formPanel);
						formPanel.getActions().add(action);
					}
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
					return;
				}
			}

			action.setName(txtName.getText());
			action.setDescription(txtDescription.getText());
			action.getRoles().clear();

			// Grant all selected roles to this action
			action.getRoles().addAll(chkViewerRoles.getCheckedElements());
		}
		else if (buttonId == IDialogConstants.INTERNAL_ID) {
			setReturnCode(DELETE);
			close();
			return;
		}

		super.buttonPressed(buttonId);
	}

}
