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

import java.util.Map;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.service.form.init.UpdateFormInitService;
import net.codecadenza.eclipse.service.integration.IntegrationBeanSyncService;
import net.codecadenza.eclipse.service.testing.gui.GUITestCaseService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPanel;
import net.codecadenza.eclipse.ui.util.validation.FormFieldCheck;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.EList;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining single-record forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditUpdateFormDialog extends CodeCadenzaTitleAreaDialog {
	private static final String DLG_TITLE_EDIT = "Edit update form";
	private static final String DLG_TITLE_NEW = "Create new update form";

	private final FormGroup formGroup;
	private final Project project;
	private final FormService formService;
	private final boolean openFormInNewWindow;
	private Text txtWidth;
	private Text txtHeight;
	private Text txtTitle;
	private Text txtDomainObject;
	private Text txtGroupName;
	private Combo cboFormType;
	private Text txtFormName;
	private DomainObjectProposalTextField propDomainObject;
	private Form form;
	private boolean doEdit;
	private FormTypeEnumeration selType;
	private Button chkModal;
	private Button chkResizable;
	private Button chkTitleArea;
	private Button chkOpenEditAfterCreate;
	private Button chkReturnVoid;
	private Label lblShareDTO;
	private Button chkShareDTO;
	private Text txtDTOName;
	private Text txtBoundaryMethod;
	private CheckboxDataGridComposite<Role> chkViewerRoles;
	private UpdateFormInitService initService;
	private VisualFormEditorPanel panPreview;
	private ScrolledComposite scrolledComposite;
	private String title = DLG_TITLE_NEW;

	/**
	 * Constructor to create new form
	 * @param parentShell
	 * @param formGroup
	 * @param project
	 */
	public EditUpdateFormDialog(Shell parentShell, FormGroup formGroup, Project project) {
		super(parentShell);

		this.formGroup = formGroup;
		this.project = project;
		this.formService = new FormService(project);
		this.openFormInNewWindow = !project.hasAngularClient() && !project.hasJSFOrVaadinClient();
	}

	/**
	 * Constructor to edit form
	 * @param parentShell
	 * @param formGroup
	 * @param project
	 * @param form
	 */
	public EditUpdateFormDialog(Shell parentShell, FormGroup formGroup, Project project, Form form) {
		this(parentShell, formGroup, project);

		this.form = form;
		this.doEdit = true;
		this.title = DLG_TITLE_EDIT;
		this.initService = new UpdateFormInitService(form);
	}

	/**
	 * Initialize the dialog based on the selected domain object
	 * @param domainObject
	 */
	private void initDialog(DomainObject domainObject) {
		boolean shareDTO = false;

		selType = FormTypeEnumeration.valueOf(cboFormType.getItem(cboFormType.getSelectionIndex()));

		if (chkShareDTO != null) {
			shareDTO = domainObject.isDTOSharingAllowed() && chkShareDTO.getSelection();

			chkShareDTO.setEnabled(domainObject.isDTOSharingAllowed());
		}

		getButton(OK).setEnabled(true);

		if (domainObject.isAbstract() && selType != FormTypeEnumeration.READONLY) {
			getButton(OK).setEnabled(false);

			final var msg = "A form of type '" + selType.getName() + "' cannot be created based on an abstract domain object!";

			MessageDialog.openInformation(getShell(), title, msg);
			return;
		}

		try {
			initService = new UpdateFormInitService(domainObject, formGroup, selType, shareDTO);
			form = initService.initializeForm();
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
			return;
		}

		final DTOBean formDTO = form.getDTO();

		chkOpenEditAfterCreate.setSelection(false);
		chkOpenEditAfterCreate.setEnabled(false);
		chkReturnVoid.setSelection(false);
		chkReturnVoid.setEnabled(false);

		if (selType == FormTypeEnumeration.CREATE || selType == FormTypeEnumeration.ADD) {
			txtBoundaryMethod.setText("create" + domainObject.getName());
			chkOpenEditAfterCreate.setEnabled(true);
			chkReturnVoid.setSelection(false);
			chkReturnVoid.setEnabled(true);
		}
		else if (selType == FormTypeEnumeration.UPDATE) {
			txtBoundaryMethod.setText("update" + domainObject.getName());
			chkReturnVoid.setSelection(true);
			chkReturnVoid.setEnabled(true);
		}
		else if (selType == FormTypeEnumeration.READONLY)
			txtBoundaryMethod.setText("find" + domainObject.getName() + "ById");

		txtFormName.setText(form.getName());
		txtTitle.setText(form.getTitle());
		chkViewerRoles.setCheckedElements(form.getRoles());

		if (openFormInNewWindow) {
			if (selType == FormTypeEnumeration.ADD) {
				// A form of this type must be modal in order to avoid display problems if the parent form is non-modal!
				chkModal.setEnabled(false);
				chkModal.setSelection(true);
			}
			else {
				chkModal.setEnabled(true);
				chkModal.setSelection(false);
			}

			chkTitleArea.setSelection(true);
			chkResizable.setSelection(form.isResizable());
			txtHeight.setText(Integer.toString(form.getHeight()));
			txtWidth.setText(Integer.toString(form.getWidth()));
		}

		if (txtDTOName != null) {
			final boolean sharedDTOExists = domainObject.getSharedDTO() != null;

			if (!chkShareDTO.getSelection())
				txtDTOName.setEnabled(true);
			else
				txtDTOName.setEnabled(!sharedDTOExists);

			txtDTOName.setText(formDTO.getName());

			if (sharedDTOExists)
				lblShareDTO.setText("Use shared DTO:");
			else
				lblShareDTO.setText("Create shared DTO:");
		}

		panPreview = new VisualFormEditorPanel(scrolledComposite, form, false, initService);

		scrolledComposite.setContent(panPreview);
		scrolledComposite.setMinSize(panPreview.computeSize(SWT.DEFAULT, SWT.DEFAULT));

		panPreview.generatePreview();
	}

	/**
	 * Initialize the dialog
	 */
	private void initDialog() {
		try {
			selType = form.getFormType();
			txtFormName.setText(form.getName());
			txtTitle.setText(form.getTitle());
			txtDomainObject.setText(form.getDomainObject().getName());
			txtGroupName.setText(form.getFormGroup().getName());
			chkViewerRoles.setCheckedElements(form.getRoles());
			chkOpenEditAfterCreate.setSelection(false);
			chkOpenEditAfterCreate.setEnabled(false);

			if (openFormInNewWindow) {
				chkTitleArea.setSelection(form.isTitleArea());
				chkModal.setSelection(form.isModal());
				chkResizable.setSelection(form.isResizable());
				txtHeight.setText(Integer.toString(form.getHeight()));
				txtWidth.setText(Integer.toString(form.getWidth()));
			}

			for (final FormAction a : form.getActions())
				if (a.getBoundaryMethod() != null && a.getType() == ActionType.CREATE
						&& !a.getBoundaryMethod().getReturnType().isVoid()) {
					chkOpenEditAfterCreate.setSelection(form.isOpenEditAfterCreate());
					chkOpenEditAfterCreate.setEnabled(true);
					break;
				}
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		groupBasicData.setLayout(new GridLayout(4, false));

		if (!doEdit) {
			final var lblDomainObject = new Label(groupBasicData, SWT.NONE);
			lblDomainObject.setText("Domain object:");

			final var gdDomainObject = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdDomainObject.widthHint = 200;

			propDomainObject = new DomainObjectProposalTextField(groupBasicData, project) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang.Object)
				 */
				@Override
				public void onProposalAccepted(DomainObject domainObject) {
					try {
						initDialog(domainObject);
					}
					catch (final Exception e) {
						CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
					}
				}
			};

			propDomainObject.setLayoutData(gdDomainObject);

			final var lblFormType = new Label(groupBasicData, SWT.NONE);
			lblFormType.setText("Form type:");

			final var gdFormType = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdFormType.widthHint = 200;

			cboFormType = new Combo(groupBasicData, SWT.READ_ONLY);
			cboFormType.setLayoutData(gdFormType);

			cboFormType.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final DomainObject domainObject = propDomainObject.getSelectedItem();

					if (domainObject == null)
						return;

					initDialog(domainObject);
				}
			});

			cboFormType.add(FormTypeEnumeration.CREATE.name());
			cboFormType.add(FormTypeEnumeration.UPDATE.name());
			cboFormType.add(FormTypeEnumeration.READONLY.name());
			cboFormType.add(FormTypeEnumeration.ADD.name());
			cboFormType.select(0);

			selType = FormTypeEnumeration.valueOf(cboFormType.getItem(cboFormType.getSelectionIndex()));

			final var lblFormName = new Label(groupBasicData, SWT.NONE);
			lblFormName.setText("Form name:");

			txtFormName = new Text(groupBasicData, SWT.BORDER);
			txtFormName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblFormTitle = new Label(groupBasicData, SWT.NONE);
			lblFormTitle.setText("Title:");

			txtTitle = new Text(groupBasicData, SWT.BORDER);
			txtTitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			if (openFormInNewWindow)
				addAdditionalControls(groupBasicData);

			final var lblMethodName = new Label(groupBasicData, SWT.NONE);
			lblMethodName.setText("Boundary method:");

			txtBoundaryMethod = new Text(groupBasicData, SWT.BORDER);
			txtBoundaryMethod.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			propDomainObject.getControl().setFocus();
		}
		else {
			final var lblFormName = new Label(groupBasicData, SWT.NONE);
			lblFormName.setText("Form name:");

			final var gdFormName = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdFormName.widthHint = 200;

			txtFormName = new Text(groupBasicData, SWT.BORDER);
			txtFormName.setLayoutData(gdFormName);
			txtFormName.setEditable(false);

			final var lblFormTitle = new Label(groupBasicData, SWT.NONE);
			lblFormTitle.setText("Title:");

			final var gdTitle = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdTitle.widthHint = 200;

			txtTitle = new Text(groupBasicData, SWT.BORDER);
			txtTitle.setLayoutData(gdTitle);

			final var lblFormGroup = new Label(groupBasicData, SWT.NONE);
			lblFormGroup.setText("Form group:");

			txtGroupName = new Text(groupBasicData, SWT.BORDER);
			txtGroupName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtGroupName.setEditable(false);

			final var lblDomainObject = new Label(groupBasicData, SWT.NONE);
			lblDomainObject.setText("Domain object:");

			txtDomainObject = new Text(groupBasicData, SWT.BORDER);
			txtDomainObject.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtDomainObject.setEditable(false);

			if (openFormInNewWindow)
				addAdditionalControls(groupBasicData);
		}

		final var lblOpenEditAfterCreate = new Label(groupBasicData, SWT.NONE);
		lblOpenEditAfterCreate.setText("Open update after create:");

		chkOpenEditAfterCreate = new Button(groupBasicData, SWT.CHECK);

		if (!doEdit) {
			chkOpenEditAfterCreate.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (chkOpenEditAfterCreate.getSelection())
						chkReturnVoid.setSelection(false);
				}
			});

			final var lblReturnVoid = new Label(groupBasicData, SWT.NONE);
			lblReturnVoid.setText("Return void:");

			chkReturnVoid = new Button(groupBasicData, SWT.CHECK);

			chkReturnVoid.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (chkReturnVoid.getSelection())
						chkOpenEditAfterCreate.setSelection(false);
				}
			});

			if (project.isBoundaryMode()) {
				lblShareDTO = new Label(groupBasicData, SWT.NONE);
				lblShareDTO.setText("Create shared DTO:");

				chkShareDTO = new Button(groupBasicData, SWT.CHECK);
				chkShareDTO.setSelection(true);

				chkShareDTO.addSelectionListener(new SelectionAdapter() {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
					 */
					@Override
					public void widgetSelected(SelectionEvent e) {
						final DomainObject domainObject = propDomainObject.getSelectedItem();

						if (domainObject == null)
							return;

						initDialog(domainObject);
					}
				});

				final var lblDTOName = new Label(groupBasicData, SWT.NONE);
				lblDTOName.setText("Form DTO:");

				txtDTOName = new Text(groupBasicData, SWT.BORDER);
				txtDTOName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			}
		}

		final var tabFolder = new TabFolder(panDialogArea, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabItemField = new TabItem(tabFolder, SWT.NONE);
		tabItemField.setText("Visual editor");

		final var panVisualEditor = new Composite(tabFolder, SWT.NONE);
		panVisualEditor.setLayout(new GridLayout());

		final var gdScrolledComposite = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdScrolledComposite.widthHint = 850;
		gdScrolledComposite.heightHint = 600;

		scrolledComposite = new ScrolledComposite(panVisualEditor, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		scrolledComposite.setLayoutData(gdScrolledComposite);
		scrolledComposite.setExpandHorizontal(true);
		scrolledComposite.setExpandVertical(true);

		if (form != null) {
			panPreview = new VisualFormEditorPanel(scrolledComposite, form, true, initService);

			scrolledComposite.setContent(panPreview);
			scrolledComposite.setMinSize(panPreview.computeSize(SWT.DEFAULT, SWT.DEFAULT));

			panPreview.generatePreview();
		}

		tabItemField.setControl(panVisualEditor);

		final var tabItemRoles = new TabItem(tabFolder, SWT.NONE);
		tabItemRoles.setText("Roles");

		final var panRoles = new Composite(tabFolder, SWT.NONE);
		panRoles.setLayout(new GridLayout());

		tabItemRoles.setControl(panRoles);

		chkViewerRoles = new CheckboxDataGridComposite<>(panRoles, REMOVE_DEFAULT_MENU_ITEMS) {
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
		chkViewerRoles.setData(project.getRoles());

		if (doEdit) {
			initDialog();

			setTitle("Edit existing single record form");
			setMessage("Change form data");
		}
		else {
			setTitle("Create new single record form");
			setMessage("Insert form data");
		}

		txtTitle.addModifyListener(e -> {
			if (panPreview == null)
				return;

			panPreview.setFormTitle(txtTitle.getText());
		});

		return panDialogArea;
	}

	/**
	 * Add additional controls for forms that are opened in a new window
	 * @param groupBasicData
	 */
	private void addAdditionalControls(final Group groupBasicData) {
		final var lblWidth = new Label(groupBasicData, SWT.NONE);
		lblWidth.setText("Width:");

		txtWidth = new Text(groupBasicData, SWT.BORDER);
		txtWidth.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblHeight = new Label(groupBasicData, SWT.NONE);
		lblHeight.setText("Height:");

		txtHeight = new Text(groupBasicData, SWT.BORDER);
		txtHeight.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblModal = new Label(groupBasicData, SWT.NONE);
		lblModal.setText("Modal:");

		chkModal = new Button(groupBasicData, SWT.CHECK);

		final var lblTitleArea = new Label(groupBasicData, SWT.NONE);
		lblTitleArea.setText("Add title area:");

		chkTitleArea = new Button(groupBasicData, SWT.CHECK);

		final var lblResizable = new Label(groupBasicData, SWT.NONE);
		lblResizable.setText("Resizable:");

		chkResizable = new Button(groupBasicData, SWT.CHECK);

		// Resize the form in the visual editor if the user changes the width or the height
		txtHeight.addModifyListener(e -> {
			if (panPreview == null)
				return;

			try {
				final int newHeight = Integer.parseInt(txtHeight.getText());

				if (newHeight > 0)
					panPreview.setFormHeight(newHeight);
			}
			catch (final NumberFormatException e1) {
				// Ignored!
			}
		});

		txtWidth.addModifyListener(e -> {
			if (panPreview == null)
				return;

			try {
				final int newWidth = Integer.parseInt(txtWidth.getText());

				if (newWidth > 0)
					panPreview.setFormWidth(newWidth);
			}
			catch (final NumberFormatException e1) {
				// Ignored!
			}
		});

		chkTitleArea.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (panPreview == null)
					return;

				panPreview.setTitleArea(chkTitleArea.getSelection());
			}
		});

		chkTitleArea.setSelection(true);
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
	 * Create and save the form
	 * @throws Exception if the creation of the form has failed
	 */
	private void insertFormData() throws Exception {
		final Map<String, DTOBean> listDTOMap = initService.getListDTOMap();
		final Map<String, DTOBean> addDTOMap = initService.getAddDTOMap();
		final Map<DTOBean, Namespace> namespaceMap = initService.getNamespaceMap();

		// Initialize the form object
		form.setName(txtFormName.getText());
		form.setTitle(txtTitle.getText());
		form.setOpenEditAfterCreate(chkOpenEditAfterCreate.getSelection());
		form.getRoles().clear();

		if (openFormInNewWindow) {
			form.setModal(chkModal.getSelection());
			form.setResizable(chkResizable.getSelection());
			form.setTitleArea(chkTitleArea.getSelection());
			form.setWidth(Integer.parseInt(txtWidth.getText()));
			form.setHeight(Integer.parseInt(txtHeight.getText()));
		}

		if (txtDTOName != null)
			form.getDTO().setName(txtDTOName.getText());

		formService.createUpdateForm(form, listDTOMap, chkViewerRoles.getCheckedElements(), formGroup, namespaceMap, addDTOMap,
				txtBoundaryMethod.getText(), chkReturnVoid.getSelection());
	}

	/**
	 * Save the form
	 * @throws Exception if the save operation has failed
	 */
	private void saveFormData() throws Exception {
		final Map<DTOBean, Namespace> namespaceMap = initService.getNamespaceMap();
		final var dtoService = new DTOBeanService(project);
		final var boundaryService = new BoundaryService(project);

		form.setTitle(txtTitle.getText());
		form.setOpenEditAfterCreate(chkOpenEditAfterCreate.isEnabled() && chkOpenEditAfterCreate.getSelection());
		form.getRoles().clear();

		if (openFormInNewWindow) {
			form.setModal(chkModal.getSelection());
			form.setTitleArea(chkTitleArea.getSelection());
			form.setResizable(chkResizable.getSelection());
			form.setWidth(Integer.parseInt(txtWidth.getText()));
			form.setHeight(Integer.parseInt(txtHeight.getText()));
		}

		// Grant all selected roles
		form.getRoles().addAll(chkViewerRoles.getCheckedElements());

		// It might be the case that there are new list DTOs that must be added
		namespaceMap.keySet().forEach(dto -> {
			final Namespace n = namespaceMap.get(dto);
			dto.setNamespace(n);
			n.getJavaTypes().add(dto);
		});

		// In case of new list fields we must add respective boundary methods!
		final EList<BoundaryBean> boundaryBuildList = formService.addAdditionalBoundaryMethods(form);

		// Save new boundaries and repositories
		boundaryBuildList.forEach(b -> {
			if (!project.eResource().getContents().contains(b))
				project.eResource().getContents().add(b);

			if (!project.eResource().getContents().contains(b.getRepository()))
				project.eResource().getContents().add(b.getRepository());
		});

		// Add the DTOs
		for (final DTOBean b : namespaceMap.keySet())
			if (!project.eResource().getContents().contains(b))
				project.eResource().getContents().add(b);

		// Synchronize integration beans
		new IntegrationBeanSyncService(project).sync();

		EclipseIDEService.saveProjectMetaData(project);

		for (final DTOBean b : namespaceMap.keySet()) {
			dtoService.rebuildDTOBeanSourceFiles(b);

			// Rebuild respective boundaries
			if (project.getBoundaryByDomainObject(b.getDomainObject()) != null)
				boundaryBuildList.add(project.getBoundaryByDomainObject(b.getDomainObject()));
		}

		// Rebuild the form DTO
		dtoService.rebuildDTOBeanSourceFiles(form.getDTO());

		// Rebuild the form
		formService.rebuildForm(form);

		// Add further boundaries that might have to be rebuilt
		for (final FormAction a : form.getActions())
			if (a.getBoundaryMethod() != null) {
				boundaryBuildList.add(a.getBoundaryMethod().getBoundaryBean());
				break;
			}

		// Rebuild boundaries
		for (final BoundaryBean b : boundaryBuildList)
			boundaryService.rebuildBoundarySourceFiles(b);

		// Synchronize GUI tests
		new GUITestCaseService(project).syncOnEditForm(form);
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean checkInput() {
		if (form == null) {
			MessageDialog.openInformation(getShell(), title, "The form must be initialized!");
			return false;
		}

		// Check form fields
		if (!FormFieldCheck.checkFormFields(getShell(), form, true))
			return false;

		// Validate the user input
		if (txtTitle.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), title, "The title must not be empty!");
			txtTitle.setFocus();
			return false;
		}

		if (openFormInNewWindow) {
			try {
				Integer.parseInt(txtWidth.getText());
			}
			catch (final NumberFormatException e) {
				MessageDialog.openInformation(getShell(), title, "The form width requires an integer value!");
				txtWidth.setFocus();
				return false;
			}

			try {
				Integer.parseInt(txtHeight.getText());
			}
			catch (final NumberFormatException e) {
				MessageDialog.openInformation(getShell(), title, "The form height requires an integer value!");
				txtHeight.setFocus();
				return false;
			}
		}

		IStatus status = EclipseIDEService.validateJavaTypeName(txtFormName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			MessageDialog.openInformation(getShell(), title, status.getMessage());
			txtFormName.setFocus();
			return false;
		}

		if (!doEdit) {
			if (project.isBoundaryMode()) {
				status = EclipseIDEService.validateJavaTypeName(txtDTOName.getText());

				if (status.getSeverity() > IStatus.INFO) {
					MessageDialog.openInformation(getShell(), title, status.getMessage());
					txtDTOName.setFocus();
					return false;
				}
			}

			status = EclipseIDEService.validateMethodName(txtBoundaryMethod.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), title, status.getMessage());
				txtBoundaryMethod.setFocus();
				return false;
			}
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (!checkInput())
				return;

			try {
				if (!doEdit)
					insertFormData();
				else
					saveFormData();
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

}
