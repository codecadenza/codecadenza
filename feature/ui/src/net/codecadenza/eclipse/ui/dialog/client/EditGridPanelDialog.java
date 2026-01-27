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

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.boundary.BoundaryService;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.form.FormService;
import net.codecadenza.eclipse.service.form.init.GridPanelInitService;
import net.codecadenza.eclipse.service.form.init.util.AssociationHelper;
import net.codecadenza.eclipse.service.integration.IntegrationBeanSyncService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import net.codecadenza.eclipse.ui.panel.DomainObjectTreePanel;
import net.codecadenza.eclipse.ui.preview.VisualFormEditorPanel;
import net.codecadenza.eclipse.ui.preview.event.PreviewChangeListener;
import net.codecadenza.eclipse.ui.util.validation.TableColumnCheck;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Dialog for creating and maintaining grid panels
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditGridPanelDialog extends CodeCadenzaTitleAreaDialog implements PreviewChangeListener {
	private static final String EJBQL_STD_TOKEN = "a.";
	private static final String ASSOCATION_KEY = "ASSOC";
	private static final String DLG_TITLE_EDIT = "Edit grid panel";
	private static final String DLG_TITLE_NEW = "Create new grid panel";

	private final FormGroup formGroup;
	private final Project project;
	private Text txtBoundaryMethod;
	private Text txtDTO;
	private Text txtCustomStatement;
	private Text txtGenStatement;
	private DomainObjectTreePanel domainObjectTreePanel;
	private Text txtTitle;
	private Text txtDomainObject;
	private Text txtGroupName;
	private Text txtPanelName;
	private DataComboViewer<AbstractDomainAssociation> cboAssoc;
	private boolean doEdit;
	private FormPanel assocPanel;
	private AbstractDomainAssociation selAssociation;
	private GridPanelInitService initService;
	private VisualFormEditorPanel panPreview;
	private ScrolledComposite scrolledComposite;
	private String title = DLG_TITLE_NEW;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param formGroup
	 */
	public EditGridPanelDialog(Shell parentShell, FormGroup formGroup) {
		super(parentShell);

		this.formGroup = formGroup;
		this.project = formGroup.findProject();
	}

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param formGroup
	 * @param formPanel
	 */
	public EditGridPanelDialog(Shell parentShell, FormGroup formGroup, FormPanel formPanel) {
		this(parentShell, formGroup);

		this.doEdit = true;
		this.selAssociation = formPanel.getAssociation();
		this.assocPanel = formPanel;
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

			// Always regenerate the select statement as there may be new join statements to be added!
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

			if (!doEdit) {
				// Create a default grid panel
				initService = new GridPanelInitService(selAssociation, rootAssociation);
				assocPanel = initService.initializeGridPanel();

				txtPanelName.setText(assocPanel.getName());
				txtTitle.setText(assocPanel.getLabel());
				txtDTO.setText(assocPanel.getDTO().getName());

				final var methodName = "get" + selAssociation.getUpperCaseName() + "Of" + selAssociation.getDomainObject().getName();
				txtBoundaryMethod.setText(methodName);
			}
			else
				initService = new GridPanelInitService(assocPanel, rootAssociation);

			txtGenStatement.setText(initService.generateSelectStatement(false));

			panPreview = new VisualFormEditorPanel(scrolledComposite, assocPanel, doEdit);
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
		selAssociation = assocPanel.getAssociation();

		txtPanelName.setText(assocPanel.getName());
		txtTitle.setText(assocPanel.getLabel());
		txtDomainObject.setText(assocPanel.getDTO().getDomainObject().getName());
		txtGroupName.setText(formGroup.getName());
		txtCustomStatement.setText(assocPanel.getBoundaryMethod().getCustomStatement());

		initDialog(assocPanel.getDTO().getDomainObject());
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
				public void onProposalAccepted(DomainObject domainObject) {
					selAssociation = null;
					cboAssoc.setData(null);

					final var assocs = new BasicEList<AbstractDomainAssociation>();

					for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations())
						if (assoc instanceof OneToManyAssociation || assoc instanceof ManyToManyAssociation)
							assocs.add(assoc);

					cboAssoc.setData(assocs);
				}
			};

			final var gdDomainObject = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdDomainObject.widthHint = 200;

			propDomainObject.setLayoutData(gdDomainObject);

			final var lblAssociation = new Label(groupBasicData, SWT.NONE);
			lblAssociation.setText("Association:");

			cboAssoc = new DataComboViewer<>(groupBasicData, SWT.READ_ONLY) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
				 */
				@Override
				public String getItemText(AbstractDomainAssociation item) {
					if (item instanceof OneToManyAssociation)
						return "Collection<" + item.getTarget().getName() + ">" + item.getName() + " [ONE_TO_MANY]";

					return "Collection<" + item.getTarget().getName() + ">" + item.getName() + " [MANY_TO_MANY]";
				}

				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
				 */
				@Override
				public void onSelectionChanged(AbstractDomainAssociation element) {
					selAssociation = element;

					initDialog(element.getTarget());
				}
			};

			final var gdAssoc = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdAssoc.widthHint = 200;

			cboAssoc.setLayoutData(gdAssoc);

			final var lblPanelName = new Label(groupBasicData, SWT.NONE);
			lblPanelName.setText("Panel name:");

			txtPanelName = new Text(groupBasicData, SWT.BORDER);
			txtPanelName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblFormTitle = new Label(groupBasicData, SWT.NONE);
			lblFormTitle.setText("Title:");

			txtTitle = new Text(groupBasicData, SWT.BORDER);
			txtTitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblDTOName = new Label(groupBasicData, SWT.NONE);
			lblDTOName.setText("DTO:");

			txtDTO = new Text(groupBasicData, SWT.BORDER);
			txtDTO.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblBoundarySearch = new Label(groupBasicData, SWT.NONE);
			lblBoundarySearch.setText("Method:");

			txtBoundaryMethod = new Text(groupBasicData, SWT.BORDER);
			txtBoundaryMethod.setEditable(true);
			txtBoundaryMethod.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}
		else {
			final var lblPanelName = new Label(groupBasicData, SWT.NONE);
			lblPanelName.setText("Panel name:");

			final var gdPanelName = new GridData(SWT.FILL, SWT.CENTER, true, false);
			gdPanelName.widthHint = 200;

			txtPanelName = new Text(groupBasicData, SWT.BORDER);
			txtPanelName.setLayoutData(gdPanelName);
			txtPanelName.setEditable(false);
			txtPanelName.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));

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
			txtDomainObject.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));

			final var lblFormGroup = new Label(groupBasicData, SWT.NONE);
			lblFormGroup.setText("Form group:");

			txtGroupName = new Text(groupBasicData, SWT.BORDER);
			txtGroupName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtGroupName.setEditable(false);
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

		domainObjectTreePanel = new DomainObjectTreePanel(panDomainObject, DomainObjectTreePanel.Mode.GRID);
		domainObjectTreePanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var panVisualEditor = new Composite(sashForm, SWT.NONE);
		tabItemField.setControl(sashForm);
		sashForm.setWeights(1, 2);

		panVisualEditor.setLayout(new GridLayout());

		final var lblVisualEditor = new Label(panVisualEditor, SWT.NONE);
		lblVisualEditor.setFont(JFaceResources.getBannerFont());
		lblVisualEditor.setText("Visual editor");

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

		if (doEdit) {
			setTitle(DLG_TITLE_EDIT);
			setMessage("Edit panel data");

			initDialog();
		}
		else {
			setTitle(DLG_TITLE_NEW);
			setMessage("Select a domain object and an association");
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

		// Receive the data in text format
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
	 * Create a new grid panel
	 * @throws Exception if the creation of a panel has failed
	 */
	private void createPanel() throws Exception {
		final String queryStatement = txtGenStatement.getText().substring(txtGenStatement.getText().indexOf("from "));
		final String custStatement = txtCustomStatement.getText();
		final Map<String, DomainAttribute> downloadAttrMap = initService.getDownloadAttrMap();
		Namespace dtoNamespace = null;

		assocPanel.setName(txtPanelName.getText());
		assocPanel.setLabel(txtTitle.getText());
		assocPanel.getDTO().setName(txtDTO.getText());
		assocPanel.setFormGroup(formGroup);

		// Set the DTO namespace
		for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
			if (ns.getName().equals(assocPanel.getDTO().getDomainObject().getNamespace().getName()))
				dtoNamespace = ns;

		final var formService = new FormService(project);
		formService.createAssociationPanel(assocPanel, selAssociation, txtBoundaryMethod.getText(), dtoNamespace, queryStatement,
				custStatement, downloadAttrMap);
	}

	/**
	 * Save the data
	 * @throws Exception if the save operation has failed
	 */
	private void savePanel() throws Exception {
		final var boundaryBuildList = new HashSet<BoundaryBean>();
		boundaryBuildList.add(assocPanel.getBoundaryMethod().getBoundaryBean());

		for (final FormAction a : assocPanel.getActions())
			if (a.getBoundaryMethod() != null)
				boundaryBuildList.add(a.getBoundaryMethod().getBoundaryBean());

		assocPanel.setLabel(txtTitle.getText());
		assocPanel.getBoundaryMethod()
				.setQueryStatement(txtGenStatement.getText().substring(txtGenStatement.getText().indexOf("from ")));
		assocPanel.getBoundaryMethod().setCustomStatement(txtCustomStatement.getText());

		// Synchronize integration beans
		new IntegrationBeanSyncService(project).sync();

		EclipseIDEService.saveProjectMetaData(project);

		// Rebuild the grid panel
		new FormService(project).rebuildGridPanel(assocPanel);

		// Rebuild the DTO
		new DTOBeanService(project).rebuildDTOBeanSourceFiles(assocPanel.getDTO());

		final var boundaryService = new BoundaryService(project);

		// Rebuild boundary beans
		for (final BoundaryBean boundary : boundaryBuildList)
			boundaryService.rebuildBoundarySourceFiles(boundary);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (assocPanel == null) {
				MessageDialog.openInformation(getShell(), title, "The grid panel must be initialized!");
				return;
			}

			// Check all table column fields and exit if the validation has failed!
			if (!TableColumnCheck.checkTableColumnFields(getShell(), FormTypeEnumeration.GRID, assocPanel.getFormTable().getFields()))
				return;

			// Validate the user input
			if (txtTitle.getText().isEmpty()) {
				MessageDialog.openInformation(getShell(), title, "The title must not be empty!");
				txtTitle.setFocus();
				return;
			}

			IStatus status = EclipseIDEService.validateJavaTypeName(txtPanelName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				setErrorMessage(status.getMessage());
				txtPanelName.setFocus();
				return;
			}

			if (!txtGenStatement.getText().contains("from ")) {
				MessageDialog.openInformation(getShell(), title, "The 'from' keyword is missing in the statement!");
				return;
			}

			if (!doEdit) {
				status = EclipseIDEService.validateJavaTypeName(txtDTO.getText());

				if (status.getSeverity() > IStatus.INFO) {
					setErrorMessage(status.getMessage());
					txtDTO.setFocus();
					return;
				}

				status = EclipseIDEService.validateMethodName(txtBoundaryMethod.getText());

				if (status.getSeverity() > IStatus.INFO) {
					setErrorMessage(status.getMessage());
					txtBoundaryMethod.setFocus();
					return;
				}

				final var uniqueActionNames = new HashSet<String>();

				// Validate the form actions
				for (final FormAction a : assocPanel.getActions()) {
					if (uniqueActionNames.contains(a.getName())) {
						setErrorMessage("A form action with the name '" + a.getName() + "' already exists!");
						return;
					}

					uniqueActionNames.add(a.getName());
				}
			}

			try {
				if (!doEdit)
					createPanel();
				else
					savePanel();
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

}
