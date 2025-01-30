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

import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;
import static net.codecadenza.eclipse.shared.Constants.IMG_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import java.util.Arrays;
import java.util.List;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeNode;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.form.TreeViewService;
import net.codecadenza.eclipse.service.form.init.util.AssociationHelper;
import net.codecadenza.eclipse.service.mapping.MappingObjectService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.util.DomainObjectProposalTextField;
import net.codecadenza.eclipse.ui.panel.DomainObjectTreePanel;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Dialog for creating and maintaining tree views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditTreeViewDialog extends CodeCadenzaTitleAreaDialog {
	private static final String TREE_VIEW_SUFFIX = "TreeView";
	private static final String TREE_VIEW_TITLE = "tree view";
	private static final String SELECT_KEY = "SELECT_NAME_KEY";
	private static final String LABEL_QUICK_SEARCH = "Quick-search items";
	private static final String LABEL_ADV_SEARCH = "Advanced search items";
	private static final String LABEL_DISPLAY_ATTRIBUTES = "Display attributes";
	private static final String LABEL_TREE_NODES = "Tree nodes";
	private static final String LABEL_SUB_TREE_ITEMS = "Sub-tree items";
	private static final String LABEL_SUB_TREE_ITEM = "Sub-tree item: ";
	private static final String LABEL_INVISIBLE_ATTR = "Invisible attributes";
	private static final String DEFAULT_INFO_MSG_STRUCT = "Define the tree view structure";
	private static final String DEFAULT_INFO_MSG_BEAN = "Select the domain object that should be the root of the tree view";
	private static final String DLG_TITLE_EDIT = "Edit tree view";
	private static final String DLG_TITLE_NEW = "Create new tree view";

	private final FormGroup formGroup;
	private final Project project;
	private TreeViewService treeViewService;
	private String title;
	private Tree treeStructure;
	private DomainObjectTreePanel domainObjectTreePanel;
	private Text txtTitle;
	private Text txtFormName;
	private Text txtMethodName;
	private Text txtCountMethodName;
	private Text txtDTOName;
	private Text txtRecursiveMethodName;
	private DTOBean treeDTO;
	private DomainAttribute selAttribute;
	private AbstractDomainAssociation selAssoc;
	private TreeView tree;
	private Menu mnuStructure;
	private Menu mnuSubItem;
	private Menu mnuRootItem;
	private Combo cboFetchType;
	private BoundaryMethodDataFetchType dataFetchType;
	private DomainObjectProposalTextField txtDomainObject;
	private Text txtQueryStatement;
	private boolean doEdit;
	private CheckboxDataGridComposite<Role> chkViewerRoles;

	/**
	 * Constructor
	 * @param parentShell
	 * @param formGroup
	 * @param project
	 */
	public EditTreeViewDialog(Shell parentShell, FormGroup formGroup, Project project) {
		super(parentShell);

		this.formGroup = formGroup;
		this.project = project;
		this.title = DLG_TITLE_NEW;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param tree
	 * @param project
	 */
	public EditTreeViewDialog(Shell parentShell, TreeView tree, Project project) {
		this(parentShell, tree.getFormGroup(), project);

		this.tree = tree;
		this.treeViewService = new TreeViewService(tree, project);
		this.doEdit = true;
		this.title = DLG_TITLE_EDIT;
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

		final var lblDomainObject = new Label(groupBasicData, SWT.NONE);
		lblDomainObject.setText("Domain object:");

		txtDomainObject = new DomainObjectProposalTextField(groupBasicData, project) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang.Object)
			 */
			@Override
			public void onProposalAccepted(DomainObject domainObject) {
				cboFetchType.removeAll();
				cboFetchType.add(BoundaryMethodDataFetchType.DEFAULT.name());
				cboFetchType.select(0);

				if (domainObject.isMandated())
					cboFetchType.add(BoundaryMethodDataFetchType.CLIENT.name());

				if (domainObject.hasUserReference())
					cboFetchType.add(BoundaryMethodDataFetchType.USER.name());

				initDialog(domainObject);
			}
		};

		final var gdDomainObject = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdDomainObject.widthHint = 200;

		txtDomainObject.setLayoutData(gdDomainObject);

		final var lblDataFetchType = new Label(groupBasicData, SWT.NONE);
		lblDataFetchType.setText("Data fetch type:");

		final var gdFetchType = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdFetchType.widthHint = 200;

		cboFetchType = new Combo(groupBasicData, SWT.READ_ONLY);
		cboFetchType.setLayoutData(gdFetchType);
		cboFetchType.add(BoundaryMethodDataFetchType.DEFAULT.name());
		cboFetchType.select(0);

		cboFetchType.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (txtDomainObject.getSelectedItem() != null)
					initDialog(txtDomainObject.getSelectedItem());
			}
		});

		final var lblFormName = new Label(groupBasicData, SWT.NONE);
		lblFormName.setLayoutData(new GridData(67, SWT.DEFAULT));
		lblFormName.setText("Name:");

		txtFormName = new Text(groupBasicData, SWT.BORDER);
		txtFormName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblFormTitle = new Label(groupBasicData, SWT.NONE);
		lblFormTitle.setText("Title:");

		txtTitle = new Text(groupBasicData, SWT.BORDER);
		txtTitle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblMethodName = new Label(groupBasicData, SWT.NONE);
		lblMethodName.setText("Boundary method:");

		txtMethodName = new Text(groupBasicData, SWT.BORDER);
		txtMethodName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblCountMethod = new Label(groupBasicData, SWT.NONE);
		lblCountMethod.setText("Count method:");

		txtCountMethodName = new Text(groupBasicData, SWT.BORDER);
		txtCountMethodName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblRecursiveMethodName = new Label(groupBasicData, SWT.NONE);
		lblRecursiveMethodName.setText("Recursive method:");

		txtRecursiveMethodName = new Text(groupBasicData, SWT.BORDER);
		txtRecursiveMethodName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblDTOName = new Label(groupBasicData, SWT.NONE);
		lblDTOName.setText("Root DTO:");

		txtDTOName = new Text(groupBasicData, SWT.BORDER);
		txtDTOName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var tabFolder = new TabFolder(panDialogArea, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabItemStruct = new TabItem(tabFolder, SWT.NONE);
		tabItemStruct.setText("Mapping");

		final var panTrees = new SashForm(tabFolder, SWT.NONE);
		panTrees.setSashWidth(4);

		final var panDomainObject = new Composite(panTrees, SWT.NONE);
		panDomainObject.setLayout(new GridLayout());

		final var lblDomainObjectStruct = new Label(panDomainObject, SWT.NONE);
		lblDomainObjectStruct.setText("Domain object hierarchy");

		tabItemStruct.setControl(panTrees);

		final var panTreeViewStruct = new Composite(panTrees, SWT.NONE);
		panTreeViewStruct.setLayout(new GridLayout());

		final var lblFormField = new Label(panTreeViewStruct, SWT.NONE);
		lblFormField.setText("Tree view structure");

		domainObjectTreePanel = new DomainObjectTreePanel(panDomainObject, DomainObjectTreePanel.Mode.TREE);
		domainObjectTreePanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		domainObjectTreePanel.setFocus();

		final var gdTreeStructure = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdTreeStructure.heightHint = 400;

		treeStructure = new Tree(panTreeViewStruct, SWT.BORDER);
		treeStructure.setLayoutData(gdTreeStructure);

		treeStructure.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();
				DTOBeanAttribute selAttr = null;

				if (selItem == null)
					return;

				try {
					if (selItem.getData() instanceof final DTOBeanAttribute dtoAttr)
						selAttr = dtoAttr;

					if (selItem.getData() instanceof final TreeNode nodeData)
						selAttr = nodeData.getDTOAttribute();

					if (selItem.getData() instanceof final TreeSearchItem treeSearchItem)
						selAttr = treeSearchItem.getDTOAttribute();

					var query = "";

					if (selAttr != null && selItem.getParentItem() != null && selItem.getParentItem().getParentItem() != null
							&& selItem.getParentItem().getParentItem().getData() != null
							&& selItem.getParentItem().getParentItem().getData() instanceof final TreeViewItem item) {
						if (item.isRootItem())
							query = treeViewService.generateSelectStatement(selAttr.getDTOBean(), null, true, false);
						else
							query = treeViewService.generateSelectStatement(selAttr.getDTOBean(), item, false, false);
					}

					if (selAttr != null && selItem.getData() instanceof TreeSearchItem)
						query = treeViewService.generateSelectStatement(treeDTO, null, true, false);

					if (selAttr == null && selItem.getParentItem() != null && selItem.getParentItem().getData() != null
							&& selItem.getParentItem().getData() instanceof final TreeViewItem item) {
						if (item.isRootItem())
							query = treeViewService.generateSelectStatement(treeDTO, null, true, false);
						else
							query = treeViewService.generateSelectStatement(item.getItemDTO(), item, false, false);
					}

					if (selAttr == null && selItem.getParentItem() != null && selItem.getData() != null
							&& selItem.getData() instanceof final TreeViewItem item) {
						if (item.isRootItem())
							query = treeViewService.generateSelectStatement(treeDTO, null, true, false);
						else
							query = treeViewService.generateSelectStatement(item.getItemDTO(), item, false, false);
					}

					txtQueryStatement.setText(query);
				}
				catch (final Exception ex) {
					setErrorMessage(ex.getMessage());
					CodeCadenzaUserInterfacePlugin.getInstance().logError(ex);
				}
			}
		});

		treeStructure.addMenuDetectListener(e -> {
			treeStructure.setMenu(null);

			final TreeItem selItem = getSelectedItem();

			if (selItem == null)
				return;

			if (selItem.getParentItem() != null) {
				final String parentItemText = selItem.getParentItem().getText();

				if (parentItemText.equals(LABEL_QUICK_SEARCH) || parentItemText.equals(LABEL_ADV_SEARCH)
						|| parentItemText.equals(LABEL_DISPLAY_ATTRIBUTES) || parentItemText.equals(LABEL_TREE_NODES)
						|| parentItemText.equals(LABEL_INVISIBLE_ATTR))
					treeStructure.setMenu(mnuStructure);
			}

			if (selItem.getData() instanceof final TreeViewItem item) {
				if (item.isRootItem())
					treeStructure.setMenu(mnuRootItem);
				else
					treeStructure.setMenu(mnuSubItem);
			}
		});

		final var treeStructureDropTarget = new DropTarget(treeStructure, DND.DROP_COPY);
		treeStructureDropTarget.setTransfer(TextTransfer.getInstance());

		treeStructureDropTarget.addDropListener(new DropTargetAdapter() {
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
				final TreeItem itemDrop = treeStructure.getItem(treeStructure.toControl(new Point(event.x, event.y)));
				final TreeItem itemDrag = domainObjectTreePanel.getSelectedItem();

				selAttribute = null;
				selAssoc = null;

				if (itemDrop == null || itemDrag == null || itemDrop.getData() == null || itemDrag.getData() == null)
					return;

				if (itemDrag.getData() instanceof final DomainAttribute domainAttribute)
					selAttribute = domainAttribute;
				else if (itemDrag.getData() instanceof final AbstractDomainAssociation assoc)
					selAssoc = assoc;

				try {
					if (selAttribute != null)
						addNode(itemDrop);

					if (selAssoc != null)
						addSubItem(itemDrop);
				}
				catch (final Exception e) {
					setErrorMessage(e.getMessage());
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		});

		final var tabItemQuery = new TabItem(tabFolder, SWT.NONE);
		tabItemQuery.setText("Query statement");

		final var panQuery = new Composite(tabFolder, SWT.NONE);
		panQuery.setLayout(new GridLayout());

		tabItemQuery.setControl(panQuery);

		txtQueryStatement = new Text(panQuery, SWT.BORDER | SWT.WRAP | SWT.V_SCROLL | SWT.CANCEL);
		txtQueryStatement.setFont(JFaceResources.getFont(JFaceResources.TEXT_FONT));
		txtQueryStatement.setEditable(false);
		txtQueryStatement.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		mnuStructure = new Menu(treeStructure);

		final var mniRenameLabel = new MenuItem(mnuStructure, SWT.NONE);
		mniRenameLabel.setText("Rename label");

		mniRenameLabel.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();
				final var msgTitle = "Rename label";

				if (selItem == null)
					return;

				if (selItem.getParentItem().getText().equals(LABEL_QUICK_SEARCH)
						|| selItem.getParentItem().getText().equals(LABEL_ADV_SEARCH)) {
					final var s = (TreeSearchItem) selItem.getData();
					final var dlg = new InputDialog(getShell(), msgTitle, "Enter a new label:", s.getLabel(),
							EditTreeViewDialog::validateLabel);

					if (dlg.open() == Dialog.OK) {
						s.setLabel(dlg.getValue());
						selItem.setText(dlg.getValue());
					}

					return;
				}

				if (selItem.getData() instanceof final TreeNode subTreeItem) {
					final var dlg = new InputDialog(getShell(), msgTitle, "Enter a new label:", subTreeItem.getLabel(),
							EditTreeViewDialog::validateLabel);

					if (dlg.open() == Dialog.OK) {
						subTreeItem.setLabel(dlg.getValue());
						selItem.setText(dlg.getValue());
					}

					return;
				}

				MessageDialog.openInformation(getShell(), msgTitle, "This operation is not supported for the selected element!");
			}
		});

		final var mniRenameDTOAttr = new MenuItem(mnuStructure, SWT.NONE);
		mniRenameDTOAttr.setText("Rename DTO attribute");

		mniRenameDTOAttr.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();
				DTOBeanAttribute attr = null;

				if (selItem == null)
					return;

				if (selItem.getData() instanceof final TreeSearchItem treeViewItem)
					attr = treeViewItem.getDTOAttribute();
				else if (selItem.getData() instanceof final TreeNode treeNode)
					attr = treeNode.getDTOAttribute();
				else if (selItem.getData() instanceof final DTOBeanAttribute dtoAttr)
					attr = dtoAttr;

				if (attr != null) {
					final String newName = renameDTOAttribute(attr);

					if (newName != null && selItem.getData() instanceof DTOBeanAttribute)
						selItem.setText(newName);
				}
			}
		});

		final var mniDelete = new MenuItem(mnuStructure, SWT.NONE);
		mniDelete.setText("Delete");
		mniDelete.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();

				if (selItem == null)
					return;

				if (selItem.getParentItem().getText().equals(LABEL_QUICK_SEARCH)) {
					if (doEdit && !treeViewService.canRemoveSearchItem()) {
						final var msg = "The quick-search item cannot be removed because this would leave an "
								+ "improper configuration of a corresponding integration method!";

						setMessage(msg);
						return;
					}

					final var treeSearchItem = (TreeSearchItem) selItem.getData();

					treeViewService.removeDTOAttribute(treeSearchItem.getDTOAttribute(), tree.getRootTreeItem());

					tree.getQuickSearchItems().remove(treeSearchItem);

					selItem.dispose();
				}
				else if (selItem.getParentItem().getText().equals(LABEL_ADV_SEARCH)) {
					if (doEdit && !treeViewService.canRemoveSearchItem()) {
						final var msg = "The advanced search item cannot be removed because this would leave an "
								+ "improper configuration of a corresponding integration method!";

						setMessage(msg);
						return;
					}

					final var treeSearchItem = (TreeSearchItem) selItem.getData();

					treeViewService.removeDTOAttribute(treeSearchItem.getDTOAttribute(), tree.getRootTreeItem());

					tree.getAdvancedSearchItems().remove(treeSearchItem);

					selItem.dispose();
				}
				else if (selItem.getParentItem().getText().equals(LABEL_DISPLAY_ATTRIBUTES)) {
					final var item = (TreeViewItem) selItem.getParentItem().getParentItem().getData();
					final var attr = (DTOBeanAttribute) selItem.getData();

					treeViewService.removeDTOAttribute(attr, item);

					item.getDisplayAttributes().remove(attr);

					selItem.dispose();
				}
				else if (selItem.getParentItem().getText().equals(LABEL_TREE_NODES)) {
					final var item = (TreeViewItem) selItem.getParentItem().getParentItem().getData();
					final var n = (TreeNode) selItem.getData();

					treeViewService.removeDTOAttribute(n.getDTOAttribute(), item);

					item.getNodes().remove(n);

					selItem.dispose();
				}
				else if (selItem.getParentItem().getText().equals(LABEL_INVISIBLE_ATTR)) {
					final var item = (TreeViewItem) selItem.getParentItem().getParentItem().getData();
					final var attr = (DTOBeanAttribute) selItem.getData();

					treeViewService.removeDTOAttribute(attr, item);

					item.getInvisibleAttributes().remove(attr);

					selItem.dispose();
				}
			}
		});

		mnuSubItem = new Menu(treeStructure);

		final var mniRenameMethod = new MenuItem(mnuSubItem, SWT.NONE);
		mniRenameMethod.setText("Rename fetch method");

		mniRenameMethod.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();

				if (selItem == null)
					return;

				if (selItem.getData() instanceof final TreeViewItem treeViewItem) {
					if (treeViewItem.isRootItem())
						return;

					final var dlgTitle = "Rename fetch method";
					final var dlgText = "Enter a new fetch method name:";
					final String methodName = treeViewItem.getDataFetchMethod().getName();
					final var dlg = new InputDialog(getShell(), dlgTitle, dlgText, methodName, EditTreeViewDialog::validateMethodName);

					if (dlg.open() == Dialog.OK)
						treeViewItem.getDataFetchMethod().setName(dlg.getValue());
				}
			}
		});

		final var mniEditStatement = new MenuItem(mnuSubItem, SWT.NONE);
		mniEditStatement.setText("Edit statement");

		mniEditStatement.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();

				if (selItem == null)
					return;

				final var subItem = (TreeViewItem) selItem.getData();
				new EditQueryStatementDialog(getShell(), subItem.getDataFetchMethod()).open();
			}
		});

		final var mniRenameDTO = new MenuItem(mnuSubItem, SWT.NONE);
		mniRenameDTO.setText("Rename DTO");

		mniRenameDTO.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedItem();

				if (selItem == null)
					return;

				final var subItem = (TreeViewItem) selItem.getData();
				final DTOBean dto = subItem.getItemDTO();

				renameDTO(dto);
			}
		});

		mnuRootItem = new Menu(treeStructure);

		final var mniEditRootStatement = new MenuItem(mnuRootItem, SWT.NONE);
		mniEditRootStatement.setText("Edit statement");

		mniEditRootStatement.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				new EditQueryStatementDialog(getShell(), tree.getBoundaryMethod(), tree.getCountMethod()).open();
			}
		});

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
			final DomainObject domainObject = tree.getDomainObject();
			treeDTO = tree.getDTO();

			addTreeViewItemsToTree(tree.getRootTreeItem(), null);

			// Initialize the domain object tree
			final AssociationHelper rootAssociationHelper = domainObjectTreePanel.init(domainObject);

			treeViewService.setRootAssociationHelper(rootAssociationHelper);

			txtFormName.setText(tree.getName());
			txtMethodName.setText(tree.getBoundaryMethod().getName());
			txtTitle.setText(tree.getTitle());
			txtDTOName.setText(treeDTO.getName());
			txtDomainObject.getControl().setText(domainObject.getName());
			txtQueryStatement.setText(treeViewService.generateSelectStatement(treeDTO, null, true, false));

			if (tree.getCountMethod() != null)
				txtCountMethodName.setText(tree.getCountMethod().getName());

			if (tree.getRecursiveMethod() != null) {
				txtRecursiveMethodName.setText(tree.getRecursiveMethod().getName());
				txtRecursiveMethodName.setEnabled(true);
			}

			dataFetchType = tree.getBoundaryMethod().getDataFetchType();

			if (domainObject.isMandated())
				cboFetchType.add(BoundaryMethodDataFetchType.CLIENT.name());

			if (domainObject.hasUserReference())
				cboFetchType.add(BoundaryMethodDataFetchType.USER.name());

			cboFetchType.select(cboFetchType.indexOf(tree.getBoundaryMethod().getDataFetchType().name()));
			txtDomainObject.getControl().setEnabled(false);
			cboFetchType.setEnabled(false);
			txtFormName.setEnabled(false);
			txtRecursiveMethodName.setEnabled(false);
			chkViewerRoles.setCheckedElements(tree.getRoles());
		}
		else {
			// Set the default roles
			chkViewerRoles.setCheckedElements(formGroup.getRoles());
		}

		setTitle(title);
		setMessage(DEFAULT_INFO_MSG_BEAN);

		txtDomainObject.getControl().setFocus();

		return null;
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
			if (!checkInput())
				return;

			// Disable the 'OK' button temporarily in order to prohibit pressing it again while performing the save operation!
			getButton(IDialogConstants.OK_ID).setEnabled(false);

			tree.setTitle(txtTitle.getText());
			tree.getRoles().clear();
			tree.getRoles().addAll(chkViewerRoles.getCheckedElements());

			if (!doEdit) {
				tree.setName(txtFormName.getText());
				tree.getBoundaryMethod().setDataFetchType(dataFetchType);
				tree.getDTO().setName(txtDTOName.getText());
				tree.setFormGroup(formGroup);
				tree.setFormType(FormTypeEnumeration.TREE_VIEW);
				tree.setHeight(0);
				tree.setModal(false);
				tree.setOpenEditAfterCreate(false);
				tree.setResizable(true);
				tree.setTitleArea(false);
			}
			else if (!treeDTO.getName().equals(txtDTOName.getText())) {
				try {
					EclipseIDEService.renameCompUnit(treeDTO.getSourceFile(), txtDTOName.getText());

					treeDTO.setName(txtDTOName.getText());
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				}
			}

			try {
				syncTreeMethods();
			}
			catch (final Exception e) {
				getButton(IDialogConstants.OK_ID).setEnabled(true);

				CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
				return;
			}

			try {
				// Save the tree view, data transfer objects and boundary beans
				treeViewService.saveTreeView(doEdit);
			}
			catch (final Exception e) {
				getButton(IDialogConstants.OK_ID).setEnabled(true);

				setErrorMessage(e.getMessage());
				CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				return;
			}
		}

		super.buttonPressed(buttonId);
	}

	/**
	 * @param itemDrop
	 */
	private void addNode(TreeItem itemDrop) {
		final TreeItem selItem = domainObjectTreePanel.getSelectedItem();

		if (selItem == null)
			return;

		final List<AbstractDomainAssociation> assocList = domainObjectTreePanel.getAssociationListOfSelectedTreeItem();
		DTOBean selDTO = null;

		if (itemDrop.getData() instanceof final DomainObject selDomainObject) {
			if (!selDomainObject.getAllValidDTOAttributes().contains(selAttribute))
				return;

			if (itemDrop.getText().equals(LABEL_QUICK_SEARCH)) {
				// Quick-search fields are only supported for attributes of type java.lang.String!
				if (!selAttribute.getJavaType().isString())
					return;

				if (doEdit && !treeViewService.canAddSearchItem()) {
					setMessage(
							"A quick-search item cannot be added because this would leave an improper configuration of a corresponding integration method!");
					return;
				}

				selDTO = treeDTO;

				final TreeSearchItem treeItem = treeViewService.initTreeSearchItem(selDTO, selAttribute, assocList,
						(String) selItem.getData(SELECT_KEY));
				tree.getQuickSearchItems().add(treeItem);

				final var itemQuickSearch = new TreeItem(itemDrop, SWT.NONE);
				itemQuickSearch.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
				itemQuickSearch.getParentItem().setExpanded(true);
				itemQuickSearch.setText(treeItem.getLabel());
				itemQuickSearch.setData(treeItem);
			}
			else if (itemDrop.getText().equals(LABEL_ADV_SEARCH)) {
				if (doEdit && !treeViewService.canAddSearchItem()) {
					final var msg = "An advanced search item cannot be added because this would leave an "
							+ "improper configuration of a corresponding integration method!";

					setMessage(msg);
					return;
				}

				selDTO = treeDTO;

				final TreeSearchItem treeItem = treeViewService.initTreeSearchItem(selDTO, selAttribute, assocList,
						(String) selItem.getData(SELECT_KEY));
				tree.getAdvancedSearchItems().add(treeItem);

				final var itemAdvSearch = new TreeItem(itemDrop, SWT.NONE);
				itemAdvSearch.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
				itemAdvSearch.getParentItem().setExpanded(true);
				itemAdvSearch.setText(treeItem.getLabel());
				itemAdvSearch.setData(treeItem);
			}
			else if (itemDrop.getText().equals(LABEL_DISPLAY_ATTRIBUTES)) {
				final var treeItem = (TreeViewItem) itemDrop.getParentItem().getData();
				selDTO = treeItem.getItemDTO();

				final DTOBeanAttribute attr = treeViewService.addAttributeToDTO(selDTO, selAttribute, assocList);
				attr.setSelectToken((String) selItem.getData(SELECT_KEY));

				if (treeItem.getDisplayAttributes().contains(attr)) {
					setMessage("The same attribute must not be added to the display attributes twice!");
					return;
				}

				treeItem.getDisplayAttributes().add(attr);

				final var itemDisplay = new TreeItem(itemDrop, SWT.NONE);
				itemDisplay.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
				itemDisplay.getParentItem().setExpanded(true);
				itemDisplay.setText(attr.getName());
				itemDisplay.setData(attr);
			}
			else if (itemDrop.getText().equals(LABEL_INVISIBLE_ATTR)) {
				final var treeItem = (TreeViewItem) itemDrop.getParentItem().getData();
				selDTO = treeItem.getItemDTO();

				final DTOBeanAttribute attr = treeViewService.addAttributeToDTO(selDTO, selAttribute, assocList);
				attr.setSelectToken((String) selItem.getData(SELECT_KEY));

				if (treeItem.getInvisibleAttributes().contains(attr)) {
					setMessage("The same attribute must not be added to the invisible attributes twice!");
					return;
				}

				treeItem.getInvisibleAttributes().add(attr);

				final var itemInvisible = new TreeItem(itemDrop, SWT.NONE);
				itemInvisible.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
				itemInvisible.getParentItem().setExpanded(true);
				itemInvisible.setText(attr.getName());
				itemInvisible.setData(attr);
			}
			else if (itemDrop.getText().equals(LABEL_TREE_NODES)) {
				final String label = treeViewService.createInitialAttributeLabel(selAttribute, assocList);
				final var treeItem = (TreeViewItem) itemDrop.getParentItem().getData();

				selDTO = treeItem.getItemDTO();

				final DTOBeanAttribute attr = treeViewService.addAttributeToDTO(selDTO, selAttribute, assocList);
				attr.setSelectToken((String) selItem.getData(SELECT_KEY));

				final TreeNode node = ClientFactory.eINSTANCE.createTreeNode();
				node.setDTOAttribute(attr);
				node.setLabel(label);

				treeItem.getNodes().add(node);

				final var itemNode = new TreeItem(itemDrop, SWT.NONE);
				itemNode.setText(label);
				itemNode.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
				itemNode.getParentItem().setExpanded(true);
				itemNode.setData(node);
			}
		}

		var fullQuery = "";

		if (selDTO != null && itemDrop.getParentItem() != null && itemDrop.getParentItem().getData() != null
				&& itemDrop.getParentItem().getData() instanceof final TreeViewItem subItem) {
			if (subItem.isRootItem()) {
				fullQuery = treeViewService.generateSelectStatement(selDTO, null, false, false);

				tree.getBoundaryMethod().setQueryStatement(treeViewService.generateSelectStatement(selDTO, null, false, true));

				if (tree.getRecursiveMethod() != null) {
					tree.getRecursiveMethod().setQueryStatement(treeViewService.generateSelectStatement(selDTO, null, true, true));
					fullQuery = treeViewService.generateSelectStatement(selDTO, null, true, false);
				}
			}
			else {
				fullQuery = treeViewService.generateSelectStatement(selDTO, subItem, false, false);
				subItem.getDataFetchMethod().setQueryStatement(treeViewService.generateSelectStatement(selDTO, subItem, false, true));
			}
		}

		txtQueryStatement.setText(fullQuery);
	}

	/**
	 * Synchronize the tree view boundary methods
	 * @throws Exception if an internal error has occurred
	 */
	private void syncTreeMethods() throws Exception {
		treeViewService.syncTreeMethod(dataFetchType);

		treeViewService.addTreeCountMethod(dataFetchType);

		tree.getBoundaryMethod().setName(txtMethodName.getText());

		if (tree.getCountMethod() != null)
			tree.getCountMethod().setName(txtCountMethodName.getText());

		if (tree.getRecursiveMethod() != null)
			tree.getRecursiveMethod().setName(txtRecursiveMethodName.getText());
	}

	/**
	 * Add a new sub-item to a given parent tree item
	 * @param itemDrop
	 */
	private void addSubItem(TreeItem itemDrop) {
		// Check if the association is allowed to be added as sub-item
		if (!itemDrop.getText().equals(LABEL_SUB_TREE_ITEMS))
			return;

		final var parentDomainObject = (DomainObject) itemDrop.getData();
		final DomainObject selDomainObject = selAssoc.getTarget();

		// Create a default item label
		String itemLabel = EclipseIDEService.buildDefaultLabel(selAssoc.getName());
		itemLabel = itemLabel.substring(0, 1).toUpperCase() + itemLabel.substring(1);

		final TreeItem parentTreeItem = itemDrop.getParentItem();
		final var parentItem = (TreeViewItem) parentTreeItem.getData();
		final TreeViewItem subTreeItem;

		try {
			subTreeItem = treeViewService.addSubTreeItem(parentItem, parentDomainObject, selAssoc);
		}
		catch (final IllegalStateException e) {
			MessageDialog.openInformation(getShell(), title, "The creation of the item is disallowed for this location!");
			return;
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
			return;
		}

		final var itemSub = new TreeItem(itemDrop, SWT.NONE);
		itemSub.setText(LABEL_SUB_TREE_ITEM + itemLabel);
		itemSub.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		itemSub.setData(subTreeItem);

		final var itemDisplayAttributes = new TreeItem(itemSub, SWT.NONE);
		itemDisplayAttributes.setText(LABEL_DISPLAY_ATTRIBUTES);
		itemDisplayAttributes.setData(selDomainObject);
		itemDisplayAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

		final var itemNodes = new TreeItem(itemSub, SWT.NONE);
		itemNodes.setText(LABEL_TREE_NODES);
		itemNodes.setData(selDomainObject);
		itemNodes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

		final var itemInvisibleAttributes = new TreeItem(itemSub, SWT.NONE);
		itemInvisibleAttributes.setText(LABEL_INVISIBLE_ATTR);
		itemInvisibleAttributes.setData(selDomainObject);
		itemInvisibleAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

		final var itemSubItems = new TreeItem(itemSub, SWT.NONE);
		itemSubItems.setText(LABEL_SUB_TREE_ITEMS);
		itemSubItems.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		itemSubItems.setData(selDomainObject);

		itemDrop.setExpanded(true);
		itemSub.setExpanded(true);
	}

	/**
	 * Validate the name of a Java method
	 * @param newText
	 * @return an error message if the validation has failed or null if the method name is valid
	 */
	private static String validateMethodName(String newText) {
		final IStatus status = EclipseIDEService.validateMethodName(newText);

		if (status.getSeverity() > IStatus.INFO)
			return status.getMessage();

		return null;
	}

	/**
	 * Validate the label of a tree view item
	 * @param newText
	 * @return an error message if the validation has failed or null if the label is valid
	 */
	private static String validateLabel(String newText) {
		if (newText.isEmpty())
			return "Please enter a text!";

		return null;
	}

	/**
	 * Initialize the dialog
	 * @param domainObject
	 */
	private void initDialog(DomainObject domainObject) {
		try {
			dataFetchType = BoundaryMethodDataFetchType.valueOf(cboFetchType.getItem(cboFetchType.getSelectionIndex()));

			treeStructure.removeAll();
			txtRecursiveMethodName.setEnabled(false);

			setErrorMessage(null);

			// Initialize the domain object tree
			final AssociationHelper rootAssociationHelper = domainObjectTreePanel.init(domainObject);

			treeViewService = new TreeViewService(project);
			treeViewService.setRootAssociationHelper(rootAssociationHelper);

			tree = treeViewService.initTreeView(domainObject);
			treeDTO = tree.getDTO();

			// Initialize the tree structure
			final var itemTree = new TreeItem(treeStructure, SWT.NONE);
			itemTree.setText("Tree view");
			itemTree.setData(domainObject);
			itemTree.setImage(CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_TREE));
			itemTree.setData(tree);

			final var itemQuickSearch = new TreeItem(itemTree, SWT.NONE);
			itemQuickSearch.setText(LABEL_QUICK_SEARCH);
			itemQuickSearch.setData(domainObject);
			itemQuickSearch.setExpanded(true);
			itemQuickSearch.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			final var itemAdvSearch = new TreeItem(itemTree, SWT.NONE);
			itemAdvSearch.setText(LABEL_ADV_SEARCH);
			itemAdvSearch.setData(domainObject);
			itemAdvSearch.setExpanded(true);
			itemAdvSearch.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			final var itemRoot = new TreeItem(itemTree, SWT.NONE);
			itemRoot.setText("Root tree item");
			itemRoot.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
			itemRoot.setData(tree.getRootTreeItem());

			final var itemDisplayAttributes = new TreeItem(itemRoot, SWT.NONE);
			itemDisplayAttributes.setText(LABEL_DISPLAY_ATTRIBUTES);
			itemDisplayAttributes.setExpanded(true);
			itemDisplayAttributes.setData(domainObject);
			itemDisplayAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			final var itemNodes = new TreeItem(itemRoot, SWT.NONE);
			itemNodes.setText(LABEL_TREE_NODES);
			itemNodes.setExpanded(true);
			itemNodes.setData(domainObject);
			itemNodes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			final var itemInvisibleAttributes = new TreeItem(itemRoot, SWT.NONE);
			itemInvisibleAttributes.setText(LABEL_INVISIBLE_ATTR);
			itemInvisibleAttributes.setExpanded(true);
			itemInvisibleAttributes.setData(domainObject);
			itemInvisibleAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			final var itemSubItems = new TreeItem(itemRoot, SWT.NONE);
			itemSubItems.setText(LABEL_SUB_TREE_ITEMS);
			itemSubItems.setExpanded(true);
			itemSubItems.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
			itemSubItems.setData(domainObject);

			itemTree.setExpanded(true);
			itemRoot.setExpanded(true);

			txtFormName.setText(domainObject.getName() + TREE_VIEW_SUFFIX);
			txtTitle.setText(
					domainObject.getLabel().substring(0, 1).toUpperCase() + domainObject.getLabel().substring(1) + " " + TREE_VIEW_TITLE);
			txtQueryStatement.setText("");
			txtMethodName.setText(tree.getBoundaryMethod().getName());
			txtCountMethodName.setText("count" + domainObject.getNamePlural() + "ForTree");
			txtDTOName.setText(treeDTO.getName());

			if (tree.getRecursiveMethod() != null) {
				txtRecursiveMethodName.setEnabled(true);
				txtRecursiveMethodName.setText(tree.getRecursiveMethod().getName());
			}

			setMessage(DEFAULT_INFO_MSG_STRUCT);
		}
		catch (final Exception e) {
			setErrorMessage(e.getMessage());
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}
	}

	/**
	 * @param treeViewItem
	 * @param parentItem
	 */
	private void addTreeViewItemsToTree(TreeViewItem treeViewItem, TreeItem parentItem) {
		TreeItem subRootItem = parentItem;
		final DomainObject domainObject = treeViewItem.getItemDTO().getDomainObject();

		if (treeViewItem.isRootItem()) {
			final var itemTree = new TreeItem(treeStructure, SWT.NONE);
			itemTree.setText("Tree view");
			itemTree.setData(tree.getDomainObject());
			itemTree.setImage(CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_TREE));
			itemTree.setData(tree);

			final var itemQuickSearch = new TreeItem(itemTree, SWT.NONE);
			itemQuickSearch.setText(LABEL_QUICK_SEARCH);
			itemQuickSearch.setData(tree.getDomainObject());
			itemQuickSearch.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			tree.getQuickSearchItems().forEach(searchItem -> {
				final var item = new TreeItem(itemQuickSearch, SWT.NONE);
				item.setText(searchItem.getLabel());
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
				item.setData(searchItem);
			});

			itemQuickSearch.setExpanded(true);

			final var itemAdvSearch = new TreeItem(itemTree, SWT.NONE);
			itemAdvSearch.setText(LABEL_ADV_SEARCH);
			itemAdvSearch.setData(tree.getDomainObject());
			itemAdvSearch.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			tree.getAdvancedSearchItems().forEach(searchItem -> {
				final var item = new TreeItem(itemAdvSearch, SWT.NONE);
				item.setText(searchItem.getLabel());
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
				item.setData(searchItem);
			});

			itemAdvSearch.setExpanded(true);

			final var rootItem = new TreeItem(itemTree, SWT.NONE);
			rootItem.setText("Root tree item");
			rootItem.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
			rootItem.setData(treeViewItem);

			subRootItem = rootItem;

			itemTree.setExpanded(true);
		}

		final var itemDisplayAttributes = new TreeItem(subRootItem, SWT.NONE);
		itemDisplayAttributes.setText(LABEL_DISPLAY_ATTRIBUTES);
		itemDisplayAttributes.setData(domainObject);
		itemDisplayAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

		treeViewItem.getDisplayAttributes().forEach(attr -> {
			final var item = new TreeItem(itemDisplayAttributes, SWT.NONE);
			item.setText(attr.getName());
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
			item.setData(attr);
		});

		final var itemNodes = new TreeItem(subRootItem, SWT.NONE);
		itemNodes.setText(LABEL_TREE_NODES);
		itemNodes.setData(domainObject);
		itemNodes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

		treeViewItem.getNodes().forEach(node -> {
			final var itemNode = new TreeItem(itemNodes, SWT.NONE);
			itemNode.setText(node.getLabel());
			itemNode.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
			itemNode.setData(node);
		});

		final var itemInvisibleAttributes = new TreeItem(subRootItem, SWT.NONE);
		itemInvisibleAttributes.setText(LABEL_INVISIBLE_ATTR);
		itemInvisibleAttributes.setData(domainObject);
		itemInvisibleAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

		treeViewItem.getInvisibleAttributes().forEach(attr -> {
			final var item = new TreeItem(itemInvisibleAttributes, SWT.NONE);
			item.setText(attr.getName());
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
			item.setData(attr);
		});

		final var itemSubItems = new TreeItem(subRootItem, SWT.NONE);
		itemSubItems.setText(LABEL_SUB_TREE_ITEMS);
		itemSubItems.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		itemSubItems.setData(domainObject);

		treeViewItem.getChildren().forEach(subItem -> {
			final var itemSub = new TreeItem(itemSubItems, SWT.NONE);
			itemSub.setText(LABEL_SUB_TREE_ITEM + subItem.getLabel());
			itemSub.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
			itemSub.setData(subItem);

			addTreeViewItemsToTree(subItem, itemSub);

			itemSub.setExpanded(true);
		});

		itemSubItems.setExpanded(true);
		subRootItem.setExpanded(true);
		itemDisplayAttributes.setExpanded(true);
		itemNodes.setExpanded(true);
		itemInvisibleAttributes.setExpanded(true);
	}

	/**
	 * Rename a data transfer object
	 * @param dto
	 */
	private void renameDTO(final DTOBean dto) {
		final var dlgTitle = "Rename DTO";
		final String oldName = dto.getName();

		final var dlg = new InputDialog(getShell(), dlgTitle, "Enter a new DTO name:", dto.getName(), newText -> {
			dto.setName(newText);

			// Validate the new name
			final IStatus status = new MappingObjectService(project).validateMappingObject(dto);

			if (status.getSeverity() > IStatus.INFO)
				return status.getMessage();

			return null;
		});

		if (dlg.open() != Window.OK) {
			dto.setName(oldName);
			return;
		}

		dto.setName(oldName);

		if (doEdit) {
			try {
				if (dto.getNamespace() != null)
					EclipseIDEService.renameCompUnit(dto.getSourceFile(), dlg.getValue());

				dto.setName(dlg.getValue());
			}
			catch (final Exception e) {
				CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
			}
		}
		else
			dto.setName(dlg.getValue());
	}

	/**
	 * Rename a DTO attribute
	 * @param dtoAttr
	 * @return the new name
	 */
	private String renameDTOAttribute(final DTOBeanAttribute dtoAttr) {
		final var dlgTitle = "Rename DTO attribute";

		final var dlg = new InputDialog(getShell(), dlgTitle, "Enter a new attribute name:", dtoAttr.getName(), newText -> {
			final DTOBean dto = dtoAttr.getDTOBean();
			final IStatus status = EclipseIDEService.validateFieldName(newText);

			if (status.getSeverity() > IStatus.INFO)
				return status.getMessage();

			for (final DTOBeanAttribute attr : dto.getAttributes()) {
				if (attr.equals(dtoAttr))
					continue;

				if (attr.getName().equals(newText))
					return "A DTO attribute with the same name already exists!";
			}

			return null;
		});

		if (dlg.open() != Window.OK)
			return null;

		dtoAttr.setName(dlg.getValue());

		return dlg.getValue();
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean checkInput() {
		IStatus status = null;

		if (tree == null) {
			setErrorMessage("The tree view must be initialized!");
			return false;
		}

		if (txtTitle.getText().isEmpty()) {
			setErrorMessage("The title must not be empty!");
			txtTitle.setFocus();
			return false;
		}

		if (!doEdit) {
			status = EclipseIDEService.validateJavaTypeName(txtFormName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				setErrorMessage(status.getMessage());
				txtFormName.setFocus();
				return false;
			}
		}

		status = EclipseIDEService.validateJavaTypeName(txtDTOName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			setErrorMessage(status.getMessage());
			txtDTOName.setFocus();
			return false;
		}

		status = EclipseIDEService.validateMethodName(txtMethodName.getText());

		if (status.getSeverity() > IStatus.INFO) {
			setErrorMessage(status.getMessage());
			txtMethodName.setFocus();
			return false;
		}

		if (!tree.getAdvancedSearchItems().isEmpty()) {
			status = EclipseIDEService.validateMethodName(txtCountMethodName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				setErrorMessage(status.getMessage());
				txtCountMethodName.setFocus();
				return false;
			}
		}

		if (txtRecursiveMethodName.isEnabled()) {
			status = EclipseIDEService.validateMethodName(txtRecursiveMethodName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				setErrorMessage(status.getMessage());
				txtRecursiveMethodName.setFocus();
				return false;
			}
		}

		return true;
	}

	/**
	 * @return the selected tree item or null if no item is selected
	 */
	private TreeItem getSelectedItem() {
		return Arrays.asList(treeStructure.getSelection()).stream().findFirst().orElse(null);
	}

}
