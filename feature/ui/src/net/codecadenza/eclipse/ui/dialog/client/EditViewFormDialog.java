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

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.service.form.init.ViewFormInitService;
import net.codecadenza.eclipse.service.form.init.util.AssociationHelper;
import net.codecadenza.eclipse.service.integration.IntegrationBeanSyncService;
import net.codecadenza.eclipse.service.testing.gui.GUITestCaseService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import net.codecadenza.eclipse.ui.panel.DomainObjectTreePanel;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPanel;
import net.codecadenza.eclipse.ui.preview.event.PreviewChangeListener;
import net.codecadenza.eclipse.ui.util.validation.TableColumnCheck;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Dialog for creating and maintaining view forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditViewFormDialog extends CodeCadenzaTitleAreaDialog implements PreviewChangeListener {
	private static final String ASSOCATION_KEY = "ASSOC";
	private static final String EJBQL_STD_TOKEN = "a.";
	private static final String DLG_TITLE_EDIT = "Edit view form";
	private static final String DLG_TITLE_NEW = "Create new view form";

	private Text txtCountMethod;
	private Text txtBoundaryMethod;
	private Text txtDTO;
	private Text txtCustomStatement;
	private Text txtGenStatement;
	private DomainObjectTreePanel domainObjectTreePanel;
	private Text txtTitle;
	private Text txtDomainObject;
	private Text txtGroupName;
	private Combo cboFormType;
	private Combo cboFetchType;
	private Text txtFormName;
	private Label lblDataFetchType;
	private DomainObject domainObject;
	private final FormGroup formGroup;
	private Form form;
	private boolean doEdit;
	private FormTypeEnumeration selType;
	private BoundaryMethodDataFetchType dataFetchType;
	private final Project project;
	private CheckboxDataGridComposite<Role> chkViewerRoles;
	private ViewFormInitService initService;
	private VisualFormEditorPanel panPreview;
	private ScrolledComposite scrolledComposite;
	private String title = DLG_TITLE_NEW;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param formGroup
	 */
	public EditViewFormDialog(Shell parentShell, FormGroup formGroup) {
		super(parentShell);

		this.formGroup = formGroup;
		this.project = formGroup.findProject();
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param formGroup
	 * @param form
	 */
	public EditViewFormDialog(Shell parentShell, FormGroup formGroup, Form form) {
		this(parentShell, formGroup);

		this.doEdit = true;
		this.form = form;
		this.title = DLG_TITLE_EDIT;
	}

	/**
	 * Add a new table column
	 * @param attribute
	 * @param selectName
	 * @param isPK
	 * @param isRefIdentifier
	 * @param visible
	 * @param assocList
	 */
	private void addTableColumn(DomainAttribute attribute, String selectName, boolean isPK, boolean isRefIdentifier,
			boolean visible, List<AbstractDomainAssociation> assocList) {
		try {
			final TableColumnField columnField = initService.addTableColumn(attribute, selectName, isPK, isRefIdentifier, visible,
					assocList, 1);

			if (columnField == null)
				return;

			txtGenStatement.setText(initService.generateSelectStatement(false));

			if (doEdit)
				setMessage("Note that the select statement must be maintained manually after adding new fields!");
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Initialize the dialog
	 * @param domainObject
	 */
	private void initDialog(DomainObject domainObject) {
		if (domainObject == null)
			return;

		if (!doEdit)
			txtGenStatement.setText("");

		try {
			final AssociationHelper rootAssociation = domainObjectTreePanel.init(domainObject);
			final String domainObjPluralName = domainObject.getNamePlural().substring(0, 1).toUpperCase()
					+ domainObject.getNamePlural().substring(1);

			if (!doEdit) {
				selType = FormTypeEnumeration.valueOf(cboFormType.getItem(cboFormType.getSelectionIndex()));
				dataFetchType = BoundaryMethodDataFetchType.valueOf(cboFetchType.getItem(cboFetchType.getSelectionIndex()));
				initService = new ViewFormInitService(domainObject, formGroup, selType, dataFetchType, rootAssociation);
				form = initService.initializeForm();

				txtFormName.setText(form.getName());
				txtTitle.setText(form.getTitle());
				txtDTO.setText(form.getDTO().getName());
				chkViewerRoles.setCheckedElements(form.getRoles());

				if (selType == FormTypeEnumeration.LOV)
					txtBoundaryMethod.setText("get" + domainObject.getUpperCaseName() + "List");
				else
					txtBoundaryMethod.setText("searchAll" + domainObjPluralName);

				if (selType == FormTypeEnumeration.SEARCHABLE_VIEW)
					txtCountMethod.setText("countAll" + domainObjPluralName);
			}
			else
				initService = new ViewFormInitService(form, rootAssociation);

			txtGenStatement.setText(initService.generateSelectStatement(false));

			panPreview = new VisualFormEditorPanel(scrolledComposite, form, doEdit, null);
			panPreview.addListener(this);

			scrolledComposite.setContent(panPreview);
			scrolledComposite.setMinSize(panPreview.computeSize(SWT.DEFAULT, SWT.DEFAULT));

			panPreview.generatePreview();

			initDropTarget(panPreview);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Initialize the dialog
	 */
	private void initDialog() {
		try {
			selType = form.getFormType();
			domainObject = form.getDomainObject();
			chkViewerRoles.setCheckedElements(form.getRoles());
			txtFormName.setText(form.getName());
			txtTitle.setText(form.getTitle());
			txtDomainObject.setText(form.getDomainObject().getName());
			txtGroupName.setText(form.getFormGroup().getName());
			txtCustomStatement.setText(form.getBoundaryMethod().getCustomStatement());
			lblDataFetchType.setText(form.getBoundaryMethod().getDataFetchType().name());

			initDialog(domainObject);
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
		final var subNamespaces = new BasicEList<Namespace>();

		project.getDomainNamespace().getChildNamespaces().forEach(subNamespaces::add);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
		groupBasicData.setLayout(new GridLayout(4, false));

		if (!doEdit) {
			final var lblDomainObject = new Label(groupBasicData, SWT.NONE);
			lblDomainObject.setText("Domain object:");

			final var propDomainObject = new DomainObjectProposalTextField(groupBasicData, project) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang.Object)
				 */
				@Override
				public void onProposalAccepted(DomainObject element) {
					domainObject = element;

					cboFetchType.removeAll();
					cboFetchType.add(BoundaryMethodDataFetchType.DEFAULT.name());
					cboFetchType.select(0);

					selType = FormTypeEnumeration.valueOf(cboFormType.getItem(cboFormType.getSelectionIndex()));

					if (selType != FormTypeEnumeration.LOV) {
						if (domainObject.isMandated())
							cboFetchType.add(BoundaryMethodDataFetchType.CLIENT.name());

						if (domainObject.hasUserReference())
							cboFetchType.add(BoundaryMethodDataFetchType.USER.name());
					}

					initDialog(domainObject);
				}
			};

			final var gdDomainObject = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdDomainObject.widthHint = 200;

			propDomainObject.setLayoutData(gdDomainObject);

			lblDataFetchType = new Label(groupBasicData, SWT.NONE);
			lblDataFetchType.setText("Data fetch type:");

			final var gdFetchType = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdFetchType.widthHint = 200;

			cboFetchType = new Combo(groupBasicData, SWT.READ_ONLY);
			cboFetchType.setLayoutData(gdFetchType);

			cboFetchType.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (domainObject == null)
						return;

					initDialog(domainObject);
				}
			});

			cboFetchType.add(BoundaryMethodDataFetchType.DEFAULT.name());
			cboFetchType.select(0);

			final var lblFormType = new Label(groupBasicData, SWT.NONE);
			lblFormType.setText("Form type:");

			cboFormType = new Combo(groupBasicData, SWT.READ_ONLY);
			cboFormType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			cboFormType.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					if (domainObject == null)
						return;

					selType = FormTypeEnumeration.valueOf(cboFormType.getItem(cboFormType.getSelectionIndex()));

					cboFetchType.removeAll();
					cboFetchType.add(BoundaryMethodDataFetchType.DEFAULT.name());
					cboFetchType.select(0);

					if (selType != FormTypeEnumeration.LOV) {
						if (domainObject.isMandated())
							cboFetchType.add(BoundaryMethodDataFetchType.CLIENT.name());

						if (domainObject.hasUserReference())
							cboFetchType.add(BoundaryMethodDataFetchType.USER.name());
					}

					initDialog(domainObject);
				}
			});

			cboFormType.add(FormTypeEnumeration.SEARCHABLE_VIEW.name());
			cboFormType.add(FormTypeEnumeration.LOV.name());
			cboFormType.add(FormTypeEnumeration.SIMPLE_VIEW.name());
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

			final var lblDTOName = new Label(groupBasicData, SWT.NONE);
			lblDTOName.setText("DTO:");

			txtDTO = new Text(groupBasicData, SWT.BORDER);
			txtDTO.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblBoundarySearch = new Label(groupBasicData, SWT.NONE);
			lblBoundarySearch.setText("Search method:");

			txtBoundaryMethod = new Text(groupBasicData, SWT.BORDER);
			txtBoundaryMethod.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblCountMethod = new Label(groupBasicData, SWT.NONE);
			lblCountMethod.setText("Count method:");

			txtCountMethod = new Text(groupBasicData, SWT.BORDER);
			txtCountMethod.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
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

			final var lblDomainObject = new Label(groupBasicData, SWT.NONE);
			lblDomainObject.setText("Domain object:");

			txtDomainObject = new Text(groupBasicData, SWT.BORDER);
			txtDomainObject.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtDomainObject.setEditable(false);

			final var lblFormGroup = new Label(groupBasicData, SWT.NONE);
			lblFormGroup.setText("Form group:");

			txtGroupName = new Text(groupBasicData, SWT.BORDER);
			txtGroupName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtGroupName.setEditable(false);

			final var lblDataFetchTypeLabel = new Label(groupBasicData, SWT.NONE);
			lblDataFetchTypeLabel.setText("Data fetch type:");

			lblDataFetchType = new Label(groupBasicData, SWT.NONE);
		}

		final var tabFolder = new TabFolder(panDialogArea, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabItemField = new TabItem(tabFolder, SWT.NONE);
		tabItemField.setText("Form editor");

		final var sashForm = new SashForm(tabFolder, SWT.NONE);

		final var panDomainObject = new Composite(sashForm, SWT.NONE);
		panDomainObject.setLayout(new GridLayout());

		final var lblDomainObject = new Label(panDomainObject, SWT.NONE);
		lblDomainObject.setFont(JFaceResources.getBannerFont());
		lblDomainObject.setText("Domain object hierarchy");

		domainObjectTreePanel = new DomainObjectTreePanel(panDomainObject, DomainObjectTreePanel.Mode.VIEW);
		domainObjectTreePanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var panVisualEditor = new Composite(sashForm, SWT.NONE);

		final var lblVisualEditor = new Label(panVisualEditor, SWT.NONE);
		lblVisualEditor.setFont(JFaceResources.getBannerFont());
		lblVisualEditor.setText("Visual editor");

		tabItemField.setControl(sashForm);
		sashForm.setWeights(1, 2);

		panVisualEditor.setLayout(new GridLayout());

		final var gdScrolledComposite = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdScrolledComposite.widthHint = 650;
		gdScrolledComposite.heightHint = 550;

		scrolledComposite = new ScrolledComposite(panVisualEditor, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		scrolledComposite.setLayoutData(gdScrolledComposite);
		scrolledComposite.setExpandHorizontal(true);
		scrolledComposite.setExpandVertical(true);

		final var tabItemSelect = new TabItem(tabFolder, SWT.NONE);
		tabItemSelect.setText("Select statement");

		final var panStatements = new Composite(tabFolder, SWT.NONE);
		panStatements.setLayout(new GridLayout());

		tabItemSelect.setControl(panStatements);

		final var lblSelect = new Label(panStatements, SWT.NONE);
		lblSelect.setText("Generated select statement:");

		txtGenStatement = new Text(panStatements, SWT.WRAP | SWT.V_SCROLL | SWT.MULTI | SWT.BORDER);
		txtGenStatement.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));
		txtGenStatement.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var lblCustomStatement = new Label(panStatements, SWT.NONE);
		lblCustomStatement.setText("Additional statement:");

		final var gdCustomStatement = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdCustomStatement.heightHint = 110;

		txtCustomStatement = new Text(panStatements, SWT.WRAP | SWT.V_SCROLL | SWT.MULTI | SWT.BORDER);
		txtCustomStatement.setLayoutData(gdCustomStatement);
		txtCustomStatement.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));

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

			setTitle(DLG_TITLE_EDIT);
			setMessage("Change form data");
		}
		else {
			setTitle(DLG_TITLE_NEW);
			setMessage("Insert form data");
		}

		txtTitle.addModifyListener(_ -> {
			if (panPreview == null)
				return;

			panPreview.setFormTitle(txtTitle.getText());
		});

		return panDialogArea;
	}

	/**
	 * @param panDropTarget
	 */
	private void initDropTarget(Composite panDropTarget) {
		// Initialize the drop target
		final var target = new DropTarget(panDropTarget, DND.DROP_COPY);
		target.setTransfer(TextTransfer.getInstance());

		target.addDropListener(new DropTargetAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				try {
					final TreeItem selItem = domainObjectTreePanel.getSelectedItem();

					if (selItem == null || selItem.getData() == null
							|| !(selItem.getData() instanceof final DomainAttribute domainAttribute))
						return;

					final TreeItem parentItem = selItem.getParentItem();

					if (parentItem.getData() instanceof AbstractDomainAssociation) {
						final var assocHelper = (AssociationHelper) parentItem.getData(ASSOCATION_KEY);
						final var selectName = assocHelper.getAlias() + "." + domainAttribute.getName();
						final List<AbstractDomainAssociation> assocList = domainObjectTreePanel.getAssociationListOfSelectedTreeItem();

						addTableColumn(domainAttribute, selectName, false, false, true, assocList);
					}
					else
						addTableColumn(domainAttribute, EJBQL_STD_TOKEN + domainAttribute.getName(), domainAttribute.isPk(), false, true,
								null);

					panPreview.generatePreview();
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.preview.event.PreviewChangeListener#onPreviewChanged()
	 */
	@Override
	public void onPreviewChanged() {
		// It could be the case that the select statement must be refreshed!
		txtGenStatement.setText(initService.generateSelectStatement(false));
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
	 * Create a new view form
	 * @throws Exception if the creation of the view has failed
	 */
	private void createView() throws Exception {
		final String queryStatement = txtGenStatement.getText().substring(txtGenStatement.getText().indexOf("from "));
		final String custStatement = txtCustomStatement.getText();
		final Map<String, DomainAttribute> downloadAttrMap = initService.getDownloadAttrMap();
		Namespace dtoNamespace = null;

		form.setName(txtFormName.getText());
		form.setTitle(txtTitle.getText());
		form.setFormGroup(formGroup);
		form.getRoles().clear();

		// Set the DTO namespace
		for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
			if (ns.getName().equals(form.getDomainObject().getNamespace().getName()))
				dtoNamespace = ns;

		// Grant all selected roles
		form.getRoles().addAll(chkViewerRoles.getCheckedElements());

		final var formService = new FormService(project);
		formService.createViewForm(form, txtBoundaryMethod.getText(), txtCountMethod.getText(), txtDTO.getText(), dtoNamespace,
				queryStatement, custStatement, downloadAttrMap, dataFetchType);
	}

	/**
	 * Save view form changes
	 * @throws Exception if the save operation has failed
	 */
	private void saveView() throws Exception {
		final FormPanel panel = form.getFormPanels().get(0);

		form.setTitle(txtTitle.getText());
		form.getRoles().clear();

		// Grant all selected roles
		form.getRoles().addAll(chkViewerRoles.getCheckedElements());

		form.getBoundaryMethod().setQueryStatement(txtGenStatement.getText().substring(txtGenStatement.getText().indexOf("from ")));
		form.getBoundaryMethod().setCustomStatement(txtCustomStatement.getText());

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			panel.getBoundaryMethod().setQueryStatement(form.getBoundaryMethod().getQueryStatement());
			panel.getBoundaryMethod().setCustomStatement(form.getBoundaryMethod().getCustomStatement());
		}

		// Synchronize integration beans
		new IntegrationBeanSyncService(project).sync();

		EclipseIDEService.saveProjectMetaData(project);

		// Rebuild the view
		new FormService(project).rebuildForm(form);

		// Rebuild the form DTO
		new DTOBeanService(project).rebuildDTOBeanSourceFiles(form.getDTO());

		// Rebuild the boundary
		new BoundaryService(project).rebuildBoundarySourceFiles(form.getBoundaryMethod().getBoundaryBean());

		// Synchronize GUI tests
		new GUITestCaseService(project).syncOnEditForm(form);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (form == null) {
				MessageDialog.openInformation(getShell(), title, "The form must be initialized!");
				return;
			}

			// Validate the user input
			if (txtTitle.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The title must not be empty!");
				txtTitle.setFocus();
				return;
			}

			IStatus status = EclipseIDEService.validateJavaTypeName(txtFormName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), title, status.getMessage());
				txtFormName.setFocus();
				return;
			}

			if (!txtGenStatement.getText().contains("from ")) {
				MessageDialog.openInformation(getShell(), title, "The 'from' keyword is missing in the statement!");
				return;
			}

			if (!doEdit) {
				final var uniqueActionNames = new HashSet<String>();

				// Validate the form actions
				for (final FormAction a : form.getActions()) {
					if (uniqueActionNames.contains(a.getName())) {
						setErrorMessage("A form action with the name '" + a.getName() + "' already exists!");
						return;
					}

					uniqueActionNames.add(a.getName());
				}

				status = EclipseIDEService.validateJavaTypeName(txtDTO.getText());

				if (status.getSeverity() > IStatus.INFO) {
					MessageDialog.openInformation(getShell(), title, status.getMessage());
					txtDTO.setFocus();
					return;
				}

				status = EclipseIDEService.validateMethodName(txtBoundaryMethod.getText());

				if (status.getSeverity() > IStatus.INFO) {
					MessageDialog.openInformation(getShell(), title, status.getMessage());
					txtBoundaryMethod.setFocus();
					return;
				}

				if (selType == FormTypeEnumeration.SEARCHABLE_VIEW) {
					status = EclipseIDEService.validateMethodName(txtCountMethod.getText());

					if (status.getSeverity() > IStatus.INFO) {
						MessageDialog.openInformation(getShell(), title, status.getMessage());
						txtCountMethod.setFocus();
						return;
					}
				}
			}

			try {
				final FormPanel panel = form.getFormPanels().get(0);

				// Check all table column fields and exit if the validation has failed!
				if (!TableColumnCheck.checkTableColumnFields(getShell(), selType, panel.getFormTable().getFields()))
					return;

				if (!doEdit)
					createView();
				else
					saveView();
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

}
