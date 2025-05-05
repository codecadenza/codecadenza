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
package net.codecadenza.eclipse.tools.reverse.editor;

import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTO_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_OTM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_OTO_ASSOC;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.dialog.AssociationTargetDialog;
import net.codecadenza.eclipse.tools.reverse.dialog.EditDomainAttributeDialog;
import net.codecadenza.eclipse.tools.reverse.dialog.EditDomainObjectDialog;
import net.codecadenza.eclipse.tools.reverse.dialog.EditJavaEnumDialog;
import net.codecadenza.eclipse.tools.reverse.dialog.EditManyToManyAssociationDialog;
import net.codecadenza.eclipse.tools.reverse.dialog.EditManyToOneAssociationDialog;
import net.codecadenza.eclipse.tools.reverse.dialog.EditOneToManyAssociationDialog;
import net.codecadenza.eclipse.tools.reverse.dialog.EditOneToOneAssociationDialog;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAssociation;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAttribute;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainObject;
import net.codecadenza.eclipse.tools.reverse.model.RevEngEnum;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringModel;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Panel that contains a tree view with all domain objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainModelTreeViewPanel extends Composite {
	private static final String DLG_TITLE_EDIT = "Edit";
	private static final String DLG_TITLE_DELETE = "Delete";
	private static final String DLG_TITLE_DELETE_OBJ = "Delete domain object";
	private static final String DLG_TITLE_ADD_ENUM = "Add enum";
	private static final String DLG_TITLE_DELETE_ENUM = "Delete enum";
	private static final String DLG_TITLE_DELETE_ATTR = "Delete domain attribute";
	private static final String DLG_TITLE_DELETE_ASSOC = "Delete domain association";
	private static final String DLG_TITLE_CONVERT_TO_OTO = "Convert to one-to-one association";
	private static final String DLG_TITLE_CONVERT_TO_OTM = "Convert to one-to-many association";
	private static final String DLG_TITLE_ADD_OTM = "Add one-to-many association";
	private static final String DLG_TITLE_EXCHANGE_OWNER = "Exchange owner";
	private static final String DLG_TITLE_CONV_TO_ATTR = "Convert to attribute";
	private static final String DLG_TITLE_CONV_TO_ASSOC = "Convert to many-to-one association";
	private static final String INVALID_ASSOC_TARGET = "???";
	private static final String DEFAULT_LIST_TYPE = "Collection";

	private final Shell shell = getDisplay().getActiveShell();
	private final Project project;
	private final List<ModelChangeListener> modelChangeListeners = new ArrayList<>();
	private Tree treeDomainModel;
	private ReverseEngineeringModel revEngModel;
	private Set<String> sequences = new HashSet<>();
	private Menu mnuNamespace;
	private Menu mnuDomainAttribute;
	private Menu mnuDomainObject;
	private Menu mnuDomainAssoc;
	private Menu mnuEnum;
	private Object selectedObject;

	/**
	 * Constructor
	 * @param panParent
	 * @param project
	 */
	public DomainModelTreeViewPanel(Composite panParent, Project project) {
		super(panParent, SWT.NONE);

		this.project = project;

		initPanel();
	}

	/**
	 * @param sequences
	 */
	public void setSequences(Set<String> sequences) {
		this.sequences = sequences;
	}

	/**
	 * @param revEngModel
	 */
	public void rebuildTree(ReverseEngineeringModel revEngModel) {
		this.revEngModel = revEngModel;

		treeDomainModel.removeAll();

		// Sort domain objects and enumerations
		revEngModel.getDomainObjects()
				.sort((obj1, obj2) -> obj1.getDomainObject().getName().compareTo(obj2.getDomainObject().getName()));

		revEngModel.getEnumerations().sort((obj1, obj2) -> obj1.getJavaEnum().getName().compareTo(obj2.getJavaEnum().getName()));

		for (final Namespace namespace : revEngModel.getNamespaces()) {
			final var itemNamespace = new TreeItem(treeDomainModel, SWT.NONE);
			itemNamespace.setText(namespace.getName());
			itemNamespace.setData(namespace);
			itemNamespace.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE));

			// Add the domain objects
			for (final RevEngDomainObject revEngObj : revEngModel.getDomainObjects()) {
				if (!revEngObj.getNamespaceName().equals(namespace.getName()))
					continue;

				final var itemObject = new TreeItem(itemNamespace, SWT.NONE);
				itemObject.setText(revEngObj.getDomainObject().getName());
				itemObject.setData(revEngObj);
				itemObject.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CLASS));

				if (!revEngObj.isCreatedByReverseEngineering())
					itemObject.setForeground(getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY));

				// Add the attributes and the associations
				addFields(itemObject, revEngObj);

				if (revEngObj.equals(selectedObject)) {
					itemObject.setExpanded(true);
					treeDomainModel.select(itemObject);
				}
			}

			// Add the enumerations
			for (final RevEngEnum revEngEnum : revEngModel.getEnumerations()) {
				if (!revEngEnum.getNamespaceName().equals(namespace.getName()))
					continue;

				final var itemEnum = new TreeItem(itemNamespace, SWT.NONE);
				itemEnum.setText(revEngEnum.getJavaEnum().getName());
				itemEnum.setData(revEngEnum);
				itemEnum.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_ENUM_DEFAULT));

				if (!revEngEnum.isCreatedByReverseEngineering())
					itemEnum.setForeground(getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY));

				// Add the literals
				revEngEnum.getJavaEnum().getEnumerationValues().forEach(literal -> {
					final var itemLiteral = new TreeItem(itemEnum, SWT.NONE);
					itemLiteral.setText(literal.getName());
					itemLiteral.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_FIELD_DEFAULT));
				});

				if (revEngEnum.equals(selectedObject))
					treeDomainModel.select(itemEnum);
			}

			itemNamespace.setExpanded(true);
		}

		// Notify all listeners that the model has been changed
		modelChangeListeners.forEach(ModelChangeListener::onModelChanged);
	}

	/**
	 * @param listener
	 */
	public void addModelChangeListener(ModelChangeListener listener) {
		modelChangeListeners.add(listener);
	}

	/**
	 * Add associations and attributes of the respective domain object to the tree view
	 * @param parentItem
	 * @param revEngObj
	 */
	private void addFields(TreeItem parentItem, RevEngDomainObject revEngObj) {
		// Sort the attributes
		revEngObj.getAttributes().sort((attr1, attr2) -> {
			final DomainAttribute attribute1 = attr1.getDomainAttribute();
			final DomainAttribute attribute2 = attr2.getDomainAttribute();
			int weightedValue1 = attribute1.isPk() ? 2 : 0;
			int weightedValue2 = attribute2.isPk() ? 2 : 0;

			if (weightedValue1 == 0)
				weightedValue1 = attribute1.isDisplayAttribute() ? 1 : 0;

			if (weightedValue2 == 0)
				weightedValue2 = attribute2.isDisplayAttribute() ? 1 : 0;

			// If both attributes don't represent neither a primary key nor a display attribute they will be sorted by using the name
			if (weightedValue1 == 0 && weightedValue2 == 0)
				return attr1.getDomainAttribute().getName().compareTo(attr2.getDomainAttribute().getName());

			// If one of both attributes represent either a primary key or a display attribute the weighted value is used for sorting
			return weightedValue2 - weightedValue1;
		});

		// Add the attributes
		revEngObj.getAttributes().forEach(revEngAttr -> {
			final DomainAttribute attr = revEngAttr.getDomainAttribute();

			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setText(attr.getTypeName() + " " + attr.getName());
			item.setData(revEngAttr);
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));

			if (attr.isPk())
				item.setForeground(getDisplay().getSystemColor(SWT.COLOR_DARK_BLUE));

			if (attr.isDisplayAttribute())
				item.setForeground(getDisplay().getSystemColor(SWT.COLOR_DARK_GREEN));

			if (!revEngAttr.isCreatedByReverseEngineering())
				item.setForeground(getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY));

			if (revEngAttr.equals(selectedObject)) {
				parentItem.setExpanded(true);
				treeDomainModel.select(item);
			}
		});

		// Sort associations by their name
		revEngObj.getAssociations()
				.sort((assoc1, assoc2) -> assoc1.getAssociation().getName().compareTo(assoc2.getAssociation().getName()));

		// Add the associations to the tree
		for (final RevEngDomainAssociation revEngAssoc : revEngObj.getAssociations()) {
			final AbstractDomainAssociation assoc = revEngAssoc.getAssociation();
			RevEngDomainAssociation reverseAssoc = null;

			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setData(revEngAssoc);

			if (assoc.getTarget() == null) {
				item.setText(INVALID_ASSOC_TARGET + " " + assoc.getName());
				item.setForeground(getDisplay().getSystemColor(SWT.COLOR_RED));

				if (assoc instanceof ManyToOneAssociation)
					item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_MTO_ASSOC));
				else
					item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_MTM_ASSOC));

				continue;
			}

			var itemText = "";

			if (assoc instanceof ManyToOneAssociation) {
				reverseAssoc = revEngAssoc.getReverseOneToManyAssoc();

				itemText = assoc.getTarget().getName() + " " + assoc.getName();
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_MTO_ASSOC));
			}
			else if (assoc instanceof OneToOneAssociation) {
				reverseAssoc = revEngAssoc.getReverseOneToOneAssoc();

				itemText = assoc.getTarget().getName() + " " + assoc.getName();
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_OTO_ASSOC));
			}
			else if (assoc instanceof OneToManyAssociation) {
				reverseAssoc = revEngAssoc.getReverseManyToOneAssoc();

				itemText = DEFAULT_LIST_TYPE + "<" + assoc.getTarget().getName() + "> " + assoc.getName();
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_OTM_ASSOC));
			}
			else if (assoc instanceof ManyToManyAssociation) {
				reverseAssoc = revEngAssoc.getReverseManyToManyAssoc();

				itemText = DEFAULT_LIST_TYPE + "<" + assoc.getTarget().getName() + "> " + assoc.getName();
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_MTM_ASSOC));
			}

			if (reverseAssoc != null)
				if (assoc.isOwner())
					itemText += " (Rev. side: " + reverseAssoc.getAssociation().getName() + ")";
				else
					itemText += " (Rev. owner: " + reverseAssoc.getAssociation().getName() + ")";

			item.setText(itemText);

			if (!revEngAssoc.isCreatedByReverseEngineering())
				item.setForeground(getDisplay().getSystemColor(SWT.COLOR_DARK_GRAY));

			if (revEngAssoc.equals(selectedObject)) {
				parentItem.setExpanded(true);
				treeDomainModel.select(item);
			}
		}
	}

	/**
	 * Initialize the panel
	 */
	private void initPanel() {
		setLayout(new FillLayout());

		treeDomainModel = new Tree(this, SWT.BORDER);

		treeDomainModel.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent event) {
				final TreeItem selItem = getSelectedTreeItem();
				Dialog dlg = null;

				if (selItem == null)
					return;

				if (selItem.getData() instanceof final RevEngDomainObject revEngDomainObj)
					dlg = new EditDomainObjectDialog(shell, revEngDomainObj, sequences, project);
				else if (selItem.getData() instanceof final RevEngEnum revEngEnum)
					dlg = new EditJavaEnumDialog(shell, revEngEnum, project);
				else if (selItem.getData() instanceof final RevEngDomainAssociation revEngAssoc) {
					if (revEngAssoc.isManyToMany())
						dlg = new EditManyToManyAssociationDialog(shell, revEngAssoc);
					else if (revEngAssoc.isOneToOne())
						dlg = new EditOneToOneAssociationDialog(shell, revEngAssoc);
					else if (revEngAssoc.isOneToMany())
						dlg = new EditOneToManyAssociationDialog(shell, revEngAssoc);
					else if (revEngAssoc.isManyToOne())
						dlg = new EditManyToOneAssociationDialog(shell, revEngAssoc);
				}
				else if (selItem.getData() instanceof final RevEngDomainAttribute revEngDomainAttr)
					dlg = new EditDomainAttributeDialog(shell, revEngDomainAttr, revEngModel, project);

				if (dlg != null && Dialog.OK == dlg.open())
					rebuildTree(revEngModel);
			}
		});

		treeDomainModel.addMenuDetectListener(event -> {
			treeDomainModel.setMenu(null);

			final TreeItem treeItem = getSelectedTreeItem();

			if (treeItem == null)
				return;

			if (treeItem.getData() instanceof RevEngDomainObject)
				treeDomainModel.setMenu(mnuDomainObject);
			else if (treeItem.getData() instanceof RevEngDomainAssociation)
				treeDomainModel.setMenu(mnuDomainAssoc);
			else if (treeItem.getData() instanceof RevEngDomainAttribute)
				treeDomainModel.setMenu(mnuDomainAttribute);
			else if (treeItem.getData() instanceof RevEngEnum)
				treeDomainModel.setMenu(mnuEnum);
			else if (treeItem.getData() instanceof Namespace)
				treeDomainModel.setMenu(mnuNamespace);
		});

		// Enable drag and drop for changing the namespace of domain objects and enumerations
		final var dragSourceDomainObject = new DragSource(treeDomainModel, DND.DROP_MOVE);
		dragSourceDomainObject.setTransfer(TextTransfer.getInstance());

		dragSourceDomainObject.addDragListener(new DragSourceAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragStart(DragSourceEvent event) {
				final TreeItem treeItem = getSelectedTreeItem();
				boolean allowDrag = false;

				if (treeItem == null)
					return;

				if (treeItem.getData() instanceof final RevEngDomainObject revEngDomainObj)
					allowDrag = revEngDomainObj.isCreatedByReverseEngineering();
				else if (treeItem.getData() instanceof final RevEngEnum revEngEnum)
					allowDrag = revEngEnum.isCreatedByReverseEngineering();

				event.doit = allowDrag;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragSetData(DragSourceEvent event) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				if (treeItem.getData() instanceof final RevEngDomainObject revEngDomainObj)
					event.data = revEngDomainObj.getDomainObject().getName();
				else if (treeItem.getData() instanceof final RevEngEnum revEngEnum)
					event.data = revEngEnum.getJavaEnum().getName();
			}
		});

		// Initialize the drop target
		final var target = new DropTarget(treeDomainModel, DND.DROP_MOVE);
		target.setTransfer(TextTransfer.getInstance());

		target.addDropListener(new DropTargetAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_MOVE;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				final TreeItem itemDrop = treeDomainModel.getItem(treeDomainModel.toControl(new Point(event.x, event.y)));

				if (itemDrop == null || itemDrop.getData() == null)
					return;

				if (!(itemDrop.getData() instanceof final Namespace namespace))
					return;

				for (final RevEngDomainObject obj : revEngModel.getDomainObjects())
					if (obj.getDomainObject().getName().equals(event.data)) {
						obj.setNamespaceName(namespace.getName());
						break;
					}

				for (final RevEngEnum obj : revEngModel.getEnumerations())
					if (obj.getJavaEnum().getName().equals(event.data)) {
						obj.setNamespaceName(namespace.getName());
						break;
					}

				rebuildTree(revEngModel);
			}
		});

		// Initialize the menus
		initializeNamespaceMenu();
		initializeDomainObjectMenu();
		initializeEnumMenu();
		initializeAttributeMenu();
		initializeDomainAssocMenu();
	}

	/**
	 * @return the selected tree item
	 */
	private TreeItem getSelectedTreeItem() {
		final TreeItem[] selItems = treeDomainModel.getSelection();
		TreeItem selItem = null;
		selectedObject = null;

		for (final TreeItem item : selItems)
			selItem = item;

		if (selItem == null || selItem.getData() == null)
			return null;

		selectedObject = selItem.getData();

		return selItem;
	}

	/**
	 * Initialize the menu for tree items that represent domain attributes
	 */
	private void initializeAttributeMenu() {
		mnuDomainAttribute = new Menu(treeDomainModel);

		final var mniEdit = new MenuItem(mnuDomainAttribute, SWT.NONE);
		mniEdit.setText(DLG_TITLE_EDIT);

		mniEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var dlg = new EditDomainAttributeDialog(shell, (RevEngDomainAttribute) treeItem.getData(), revEngModel, project);

				if (dlg.open() == Dialog.OK)
					rebuildTree(revEngModel);
			}
		});

		final var mniConvert = new MenuItem(mnuDomainAttribute, SWT.NONE);
		mniConvert.setText(DLG_TITLE_CONV_TO_ASSOC);

		mniConvert.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngAttr = (RevEngDomainAttribute) treeItem.getData();

				if (convertAttributeToAssociation(revEngAttr))
					rebuildTree(revEngModel);
			}
		});

		final var mniDelete = new MenuItem(mnuDomainAttribute, SWT.NONE);
		mniDelete.setText(DLG_TITLE_DELETE);

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngAttr = (RevEngDomainAttribute) treeItem.getData();

				if (deleteDomainAttribute(revEngAttr)) {
					selectedObject = revEngAttr.getParentObject();
					rebuildTree(revEngModel);
				}
			}
		});
	}

	/**
	 * Initialize the menu for tree items that represent enumerations
	 */
	private void initializeEnumMenu() {
		mnuEnum = new Menu(treeDomainModel);

		final var mniEdit = new MenuItem(mnuEnum, SWT.NONE);
		mniEdit.setText(DLG_TITLE_EDIT);

		mniEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngEnum = (RevEngEnum) treeItem.getData();
				final var dlg = new EditJavaEnumDialog(shell, revEngEnum, project);

				if (dlg.open() == Dialog.OK)
					rebuildTree(revEngModel);
			}
		});

		final var mniDelete = new MenuItem(mnuEnum, SWT.NONE);
		mniDelete.setText(DLG_TITLE_DELETE);

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngEnum = (RevEngEnum) treeItem.getData();

				if (deleteEnumeration(revEngEnum))
					rebuildTree(revEngModel);
			}
		});
	}

	/**
	 * Initialize the menu for tree items that represent a namespace
	 */
	private void initializeNamespaceMenu() {
		mnuNamespace = new Menu(treeDomainModel);

		final var mniAddEnum = new MenuItem(mnuNamespace, SWT.NONE);
		mniAddEnum.setText(DLG_TITLE_ADD_ENUM);

		mniAddEnum.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var namespace = (Namespace) treeItem.getData();
				final var dlg = new EditJavaEnumDialog(shell, project);

				if (dlg.open() == Dialog.OK) {
					// Add a new Java enum to the model
					revEngModel.addEnumeration(namespace.getName(), dlg.getJavaEnum(), true);

					// Refresh the tree view
					rebuildTree(revEngModel);
				}
			}
		});
	}

	/**
	 * Initialize the menu for tree items that represent domain objects
	 */
	private void initializeDomainObjectMenu() {
		mnuDomainObject = new Menu(treeDomainModel);

		final var mniEdit = new MenuItem(mnuDomainObject, SWT.NONE);
		mniEdit.setText(DLG_TITLE_EDIT);

		mniEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngObj = (RevEngDomainObject) treeItem.getData();
				final var dlg = new EditDomainObjectDialog(shell, revEngObj, sequences, project);

				if (dlg.open() == Dialog.OK)
					rebuildTree(revEngModel);
			}
		});

		final var mniDelete = new MenuItem(mnuDomainObject, SWT.NONE);
		mniDelete.setText(DLG_TITLE_DELETE);

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngObj = (RevEngDomainObject) treeItem.getData();

				if (deleteDomainObject(revEngObj))
					rebuildTree(revEngModel);
			}
		});
	}

	/**
	 * Initialize the menu for tree items that represent domain objects
	 */
	private void initializeDomainAssocMenu() {
		mnuDomainAssoc = new Menu(treeDomainModel);

		final var mniEdit = new MenuItem(mnuDomainAssoc, SWT.NONE);
		mniEdit.setText(DLG_TITLE_EDIT);

		mniEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();
				Dialog dlg = null;

				if (treeItem == null)
					return;

				final var revEngAssoc = (RevEngDomainAssociation) treeItem.getData();

				if (revEngAssoc.isManyToMany())
					dlg = new EditManyToManyAssociationDialog(shell, revEngAssoc);
				else if (revEngAssoc.isOneToOne())
					dlg = new EditOneToOneAssociationDialog(shell, revEngAssoc);
				else if (revEngAssoc.isOneToMany())
					dlg = new EditOneToManyAssociationDialog(shell, revEngAssoc);
				else if (revEngAssoc.isManyToOne())
					dlg = new EditManyToOneAssociationDialog(shell, revEngAssoc);

				if (dlg != null && dlg.open() == Dialog.OK)
					rebuildTree(revEngModel);
			}
		});

		final var mniExchange = new MenuItem(mnuDomainAssoc, SWT.NONE);
		mniExchange.setText(DLG_TITLE_EXCHANGE_OWNER);

		mniExchange.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngAssoc = (RevEngDomainAssociation) treeItem.getData();

				if (exchangeAssociationOwner(revEngAssoc))
					rebuildTree(revEngModel);
			}
		});

		final var mniAdd = new MenuItem(mnuDomainAssoc, SWT.NONE);
		mniAdd.setText(DLG_TITLE_ADD_OTM);

		mniAdd.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngAssoc = (RevEngDomainAssociation) treeItem.getData();

				if (addOneToManyAssociation(revEngAssoc))
					rebuildTree(revEngModel);
			}
		});

		final var mniConvertToOneToOne = new MenuItem(mnuDomainAssoc, SWT.NONE);
		mniConvertToOneToOne.setText(DLG_TITLE_CONVERT_TO_OTO);

		mniConvertToOneToOne.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngAssoc = (RevEngDomainAssociation) treeItem.getData();

				if (convertToOneToOneAssociation(revEngAssoc))
					rebuildTree(revEngModel);
			}
		});

		final var mniConvertToOneToMany = new MenuItem(mnuDomainAssoc, SWT.NONE);
		mniConvertToOneToMany.setText(DLG_TITLE_CONVERT_TO_OTM);

		mniConvertToOneToMany.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngAssoc = (RevEngDomainAssociation) treeItem.getData();

				if (convertToOneToManyAssociation(revEngAssoc))
					rebuildTree(revEngModel);
			}
		});

		final var mniConvToAttr = new MenuItem(mnuDomainAssoc, SWT.NONE);
		mniConvToAttr.setText(DLG_TITLE_CONV_TO_ATTR);

		mniConvToAttr.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngAssoc = (RevEngDomainAssociation) treeItem.getData();

				if (convertAssociationToAttribute(revEngAssoc))
					rebuildTree(revEngModel);
			}
		});

		final var mniDelete = new MenuItem(mnuDomainAssoc, SWT.NONE);
		mniDelete.setText(DLG_TITLE_DELETE);

		mniDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem treeItem = getSelectedTreeItem();

				if (treeItem == null)
					return;

				final var revEngAssoc = (RevEngDomainAssociation) treeItem.getData();

				if (deleteDomainAssociation(revEngAssoc, false)) {
					selectedObject = revEngAssoc.getParentObject();
					rebuildTree(revEngModel);
				}
			}
		});
	}

	/**
	 * Delete the domain object
	 * @param revEngObj
	 * @return true if the delete operation was finished successfully
	 */
	private boolean deleteDomainObject(RevEngDomainObject revEngObj) {
		final DomainObject domainObject = revEngObj.getDomainObject();

		if (!revEngObj.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_DELETE_OBJ, "An existing domain object cannot be deleted!");
			return false;
		}

		final boolean doIt = MessageDialog.openConfirm(getShell(), DLG_TITLE_DELETE_OBJ,
				"Do you really want to delete the selected domain object?");

		if (!doIt)
			return false;

		final var assocList = new ArrayList<RevEngDomainAssociation>();

		// Search for all associations that reference the selected domain object and delete them!
		for (final RevEngDomainObject obj : revEngModel.getDomainObjects()) {
			if (obj.getDomainObject().equals(domainObject))
				continue;

			obj.getAssociations().forEach(revEngAssoc -> {
				final AbstractDomainAssociation assoc = revEngAssoc.getAssociation();

				if (assoc.getTarget() != null && assoc.getTarget().equals(domainObject))
					assocList.add(revEngAssoc);
			});
		}

		for (final RevEngDomainAssociation revEngAssoc : assocList) {
			final boolean success = deleteDomainAssociation(revEngAssoc, true);

			if (!success)
				return false;
		}

		// Remove the domain object from the model
		revEngModel.getDomainObjects().remove(revEngObj);

		return true;
	}

	/**
	 * Delete the enumeration
	 * @param revEngEnum
	 * @return true if the delete operation was finished successfully
	 */
	private boolean deleteEnumeration(RevEngEnum revEngEnum) {
		if (!revEngEnum.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_DELETE_ENUM, "An existing enumeration cannot be deleted!");
			return false;
		}

		final JavaEnum javaEnum = revEngEnum.getJavaEnum();

		for (final RevEngDomainObject revEngObj : revEngModel.getDomainObjects())
			for (final RevEngDomainAttribute attr : revEngObj.getAttributes())
				if (attr.getDomainAttribute().getJavaType().equals(javaEnum)) {
					final var msg = "An enumeration cannot be deleted as long as an attribute uses this type!";
					MessageDialog.openInformation(getShell(), DLG_TITLE_DELETE_ENUM, msg);
					return false;
				}

		revEngModel.getEnumerations().remove(revEngEnum);

		return true;
	}

	/**
	 * Delete the domain attribute
	 * @param revEngAttribute
	 * @return true if the delete operation was finished successfully
	 */
	private boolean deleteDomainAttribute(RevEngDomainAttribute revEngAttribute) {
		final DomainAttribute attribute = revEngAttribute.getDomainAttribute();

		if (!revEngAttribute.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_DELETE_ATTR, "An existing domain attribute cannot be deleted!");
			return false;
		}

		if (attribute.isPk()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_DELETE_ATTR, "The primary key attribute must not be deleted!");
			return false;
		}

		final boolean doIt = MessageDialog.openConfirm(getShell(), DLG_TITLE_DELETE_ATTR,
				"Do you really want to delete the selected attribute?");

		if (!doIt)
			return false;

		revEngModel.deleteDomainAttribute(revEngAttribute);

		return true;
	}

	/**
	 * Add a one-to-many association to the existing many-to-one association
	 * @param revEngAssoc
	 * @return true if the one-to-many association was added successfully
	 */
	private boolean addOneToManyAssociation(RevEngDomainAssociation revEngAssoc) {
		final AbstractDomainAssociation assoc = revEngAssoc.getAssociation();
		final RevEngDomainObject revEngObj = revEngAssoc.getParentObject();

		if (!revEngAssoc.isManyToOne()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_ADD_OTM,
					"A one-to-many association can only be added to a many-to-one association!");
			return false;
		}

		if (!revEngAssoc.isCreatedByReverseEngineering()) {
			final var msg = "A one-to-many association cannot be added as an existing association must not be changed!";

			MessageDialog.openInformation(getShell(), DLG_TITLE_ADD_OTM, msg);
			return false;
		}

		if (!assoc.isOwner()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_ADD_OTM, "The many-to-one association must be the owner!");
			return false;
		}

		if (assoc.getTarget() == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_ADD_OTM, "The many-to-one association has no target!");
			return false;
		}

		final DomainObject domainObject = revEngObj.getDomainObject();
		final String otmName = domainObject.getNamePlural().substring(0, 1).toLowerCase() + domainObject.getNamePlural().substring(1);

		final var mto = (ManyToOneAssociation) assoc;
		mto.setOwner(false);

		final OneToManyAssociation otm = DomainFactory.eINSTANCE.createOneToManyAssociation();
		otm.setTarget(domainObject);
		otm.setOwner(true);
		otm.setTag(AssociationTagEnumeration.NONE);
		otm.setName(otmName);
		otm.setReverseAssociation(mto);
		otm.setFetchTypeEager(false);
		otm.setCascadeMerge(true);
		otm.setCascadePersist(true);
		otm.setCascadeRefresh(true);
		otm.setCascadeRemove(true);

		mto.setReverseAssociation(otm);

		// Add the association to the domain object model
		for (final RevEngDomainObject obj : revEngModel.getDomainObjects())
			if (obj.getDomainObject().equals(mto.getTarget())) {
				obj.addAssociation(otm, true);
				break;
			}

		return true;
	}

	/**
	 * Exchange the owner of bidirectional many-to-many associations
	 * @param revEngAssoc
	 * @return true if the operation was finished successfully
	 */
	private boolean exchangeAssociationOwner(RevEngDomainAssociation revEngAssoc) {
		final AbstractDomainAssociation assoc = revEngAssoc.getAssociation();

		if (!revEngAssoc.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_EXCHANGE_OWNER,
					"The owner for existing domain associations cannot be changed!");
			return false;
		}

		if (!(assoc instanceof final ManyToManyAssociation mtm)) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_EXCHANGE_OWNER,
					"This operation is only permitted for many-to-many associations!");
			return false;
		}

		final RevEngDomainAssociation reverseAssoc = revEngAssoc.getReverseManyToManyAssoc();
		final DBTable assocTable;

		if (!mtm.isBidirectional() || reverseAssoc == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_EXCHANGE_OWNER, "The association is not bidirectional!");
			return false;
		}

		final var revManyToMany = (ManyToManyAssociation) reverseAssoc.getAssociation();

		if (mtm.isOwner())
			assocTable = mtm.getTable();
		else
			assocTable = revManyToMany.getTable();

		if (assocTable == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_EXCHANGE_OWNER, "A many-to-many association table could not be found!");
			return false;
		}

		if (mtm.isOwner()) {
			revManyToMany.setTable(assocTable);
			revManyToMany.setOwner(true);
			mtm.setTable(null);
			mtm.setOwner(false);
		}
		else {
			mtm.setTable(assocTable);
			mtm.setOwner(true);
			revManyToMany.setOwner(false);
			revManyToMany.setTable(null);
		}

		DBColumn col1 = null;
		DBColumn col2 = null;

		for (final DBColumn col : assocTable.getColumns()) {
			if (assocTable.getPrimaryKey() != null && col.equals(assocTable.getPrimaryKey().getColumn()))
				continue;

			if (col1 == null)
				col1 = col;
			else
				col2 = col;
		}

		// The right column order is important as the first column represents the join column. The second column is used as the
		// inverse join column!
		assocTable.getColumns().remove(col1);
		assocTable.getColumns().remove(col2);
		assocTable.getColumns().add(col2);
		assocTable.getColumns().add(col1);

		return true;
	}

	/**
	 * Convert an association to a bidirectional one-to-one association
	 * @param revEngAssoc
	 * @return true if the conversion was finished successfully
	 */
	private boolean convertToOneToOneAssociation(RevEngDomainAssociation revEngAssoc) {
		final AbstractDomainAssociation assoc = revEngAssoc.getAssociation();
		final RevEngDomainObject revEngObj = revEngAssoc.getParentObject();

		if (!revEngAssoc.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONVERT_TO_OTO, "An existing domain association cannot be converted!");
			return false;
		}

		if (!revEngAssoc.isManyToOne()) {
			final var msg = "Only a many-to-one association can be converted to a one-to-one association!";

			MessageDialog.openInformation(getShell(), DLG_TITLE_CONVERT_TO_OTO, msg);
			return false;
		}

		if (!assoc.isOwner()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONVERT_TO_OTO, "The many-to-one association must be the owner!");
			return false;
		}

		if (assoc.getTarget() == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONVERT_TO_OTO, "The many-to-one association has no target!");
			return false;
		}

		final DomainObject domainObject = revEngObj.getDomainObject();
		final var mto = (ManyToOneAssociation) assoc;
		final String reverseName = domainObject.getLowerCaseName();

		final OneToOneAssociation oto = DomainFactory.eINSTANCE.createOneToOneAssociation();
		oto.setColumn(mto.getColumn());
		oto.setTarget(mto.getTarget());
		oto.setOptional(mto.isOptional());
		oto.setOwner(true);
		oto.setTag(AssociationTagEnumeration.NONE);
		oto.setName(mto.getName());
		oto.setFetchTypeEager(mto.isFetchTypeEager());
		oto.setCascadeMerge(true);
		oto.setCascadePersist(true);
		oto.setCascadeRefresh(true);
		oto.setCascadeRemove(true);

		// Create the reverse side of a one-to-one association
		final OneToOneAssociation reverseAssoc = DomainFactory.eINSTANCE.createOneToOneAssociation();
		reverseAssoc.setTarget(domainObject);
		reverseAssoc.setOptional(mto.isOptional());
		reverseAssoc.setOwner(false);
		reverseAssoc.setTag(AssociationTagEnumeration.NONE);
		reverseAssoc.setName(reverseName);
		reverseAssoc.setReverseAssociation(oto);
		reverseAssoc.setFetchTypeEager(mto.isFetchTypeEager());
		reverseAssoc.setCascadeMerge(true);
		reverseAssoc.setCascadePersist(true);
		reverseAssoc.setCascadeRefresh(true);
		reverseAssoc.setCascadeRemove(true);

		oto.setReverseAssociation(reverseAssoc);

		// Add the association to the domain object model
		for (final RevEngDomainObject obj : revEngModel.getDomainObjects())
			if (obj.getDomainObject().equals(oto.getTarget())) {
				obj.addAssociation(reverseAssoc, true);
				break;
			}

		revEngObj.addAssociation(oto, true);
		revEngObj.getAssociations().remove(revEngAssoc);

		return true;
	}

	/**
	 * Convert an association to a unidirectional one-to-many association
	 * @param revEngAssoc
	 * @return true if the conversion was finished successfully
	 */
	private boolean convertToOneToManyAssociation(RevEngDomainAssociation revEngAssoc) {
		final AbstractDomainAssociation assoc = revEngAssoc.getAssociation();
		final RevEngDomainObject revEngObj = revEngAssoc.getParentObject();

		if (!revEngAssoc.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONVERT_TO_OTM, "An existing domain association cannot be converted!");
			return false;
		}

		if (!revEngAssoc.isManyToMany()) {
			final var msg = "Only a many-to-many association can be converted to a one-to-many association!";

			MessageDialog.openInformation(getShell(), DLG_TITLE_CONVERT_TO_OTM, msg);
			return false;
		}

		if (!assoc.isOwner()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONVERT_TO_OTM, "The many-to-many association must be the owner!");
			return false;
		}

		if (assoc.getTarget() == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONVERT_TO_OTM, "The many-to-many association has no target!");
			return false;
		}

		final var mtm = (ManyToManyAssociation) assoc;

		final OneToManyAssociation otm = DomainFactory.eINSTANCE.createOneToManyAssociation();
		otm.setTable(mtm.getTable());
		otm.setTarget(mtm.getTarget());
		otm.setOwner(true);
		otm.setTag(AssociationTagEnumeration.NONE);
		otm.setName(mtm.getName());
		otm.setFetchTypeEager(mtm.isFetchTypeEager());
		otm.setCascadeMerge(false);
		otm.setCascadePersist(false);
		otm.setCascadeRefresh(true);
		otm.setCascadeRemove(false);

		final RevEngDomainAssociation reverseAssoc = revEngAssoc.getReverseManyToManyAssoc();

		if (reverseAssoc != null) {
			// Remove the reverse many-to-many association from the model
			final RevEngDomainObject reverseObj = reverseAssoc.getParentObject();
			reverseObj.getAssociations().remove(reverseAssoc);
		}

		revEngObj.addAssociation(otm, true);
		revEngObj.getAssociations().remove(revEngAssoc);

		return true;
	}

	/**
	 * Delete the domain association
	 * @param revEngAssoc
	 * @param skipConfirm
	 * @return true if the delete operation was finished successfully
	 */
	private boolean deleteDomainAssociation(RevEngDomainAssociation revEngAssoc, boolean skipConfirm) {
		if (revEngAssoc == null)
			return false;

		if (!revEngAssoc.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_DELETE_ASSOC, "An existing domain association cannot be deleted!");
			return false;
		}

		boolean doIt = true;

		if (!skipConfirm)
			doIt = MessageDialog.openConfirm(getShell(), DLG_TITLE_DELETE_ASSOC,
					"Do you really want to delete the selected association?");

		if (!doIt)
			return false;

		revEngModel.deleteDomainAssociation(revEngAssoc);

		return true;
	}

	/**
	 * @param revEngAttr
	 * @return true if the conversion operation was finished successfully
	 */
	private boolean convertAttributeToAssociation(RevEngDomainAttribute revEngAttr) {
		if (revEngAttr == null)
			return false;

		final RevEngDomainObject revEngObj = revEngAttr.getParentObject();
		final DomainAttribute attr = revEngAttr.getDomainAttribute();
		final DBColumn column = attr.getColumn();

		if (!revEngAttr.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONV_TO_ASSOC, "An existing domain attribute cannot be converted!");
			return false;
		}

		if (attr.isPk()) {
			final var msg = "A domain attribute that is mapped to the primary key column cannot be converted!";

			MessageDialog.openInformation(getShell(), DLG_TITLE_CONV_TO_ASSOC, msg);
			return false;
		}

		if (column == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONV_TO_ASSOC, "The attribute is not mapped to a column!");
			return false;
		}

		final boolean doIt = MessageDialog.openConfirm(getShell(), DLG_TITLE_CONV_TO_ASSOC,
				"Do you really want to convert the selected attribute?");

		if (!doIt)
			return false;

		final var dlg = new AssociationTargetDialog(getShell(), attr, revEngModel);
		DomainObject targetDomainObject = null;

		if (dlg.open() == Dialog.OK)
			targetDomainObject = dlg.getSelectedDomainObject();

		if (targetDomainObject == null)
			return false;

		final ManyToOneAssociation assoc = DomainFactory.eINSTANCE.createManyToOneAssociation();
		assoc.setColumn(column);
		assoc.setInsertable(true);
		assoc.setUpdatable(true);
		assoc.setOptional(column.isNullable());
		assoc.setOwner(true);
		assoc.setTag(AssociationTagEnumeration.NONE);
		assoc.setName(attr.getName());
		assoc.setTarget(targetDomainObject);

		revEngObj.addAssociation(assoc, true);
		revEngObj.removeAttribute(revEngAttr);

		return true;
	}

	/**
	 * @param revEngAssoc
	 * @return true if the conversion operation was finished successfully
	 */
	private boolean convertAssociationToAttribute(RevEngDomainAssociation revEngAssoc) {
		if (revEngAssoc == null)
			return false;

		if (!revEngAssoc.isCreatedByReverseEngineering()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONV_TO_ATTR, "An existing domain association cannot be converted!");
			return false;
		}

		if (!revEngAssoc.isManyToOne()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONV_TO_ATTR,
					"Only a many-to-one association can be converted to an attribute!");
			return false;
		}

		final var mto = (ManyToOneAssociation) revEngAssoc.getAssociation();

		if (mto.getColumn() == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONV_TO_ATTR, "The many-to-one association is not mapped to a column!");
			return false;
		}

		final boolean doIt = MessageDialog.openConfirm(getShell(), DLG_TITLE_CONV_TO_ATTR,
				"Do you really want to convert the selected association?");

		if (!doIt)
			return false;

		final RevEngDomainObject revEngObj = revEngAssoc.getParentObject();
		final DBColumn column = mto.getColumn();
		final DBColumnType colType = column.getColumnType();
		final String tableName = column.getDatabaseTable().getName();
		final String columnName = column.getName();

		// Create a new domain attribute
		final DomainAttribute attr = DomainFactory.eINSTANCE.createDomainAttribute();
		attr.setColumn(column);
		attr.setInsertable(true);
		attr.setUpdatable(true);
		attr.setName(mto.getName());
		attr.setLabel(EclipseIDEService.buildDefaultLabel(attr.getName()));
		attr.setLabelPlural(EclipseIDEService.buildDefaultPluralLabel(attr.getName()));
		attr.setPersistent(true);
		attr.setTag(AttributeTagEnumeration.NONE);
		attr.setFetchTypeEager(true);

		if (colType.getJavaTypes().isEmpty()) {
			final var msgText = "A valid Java type for column '" + columnName + "' of table '" + tableName + "' could not be found!";
			MessageDialog.openInformation(getShell(), DLG_TITLE_CONV_TO_ATTR, msgText);
			return false;
		}

		attr.setJavaType(colType.getJavaTypes().get(0));

		if (attr.getJavaType().isDateOrCalendar())
			attr.setTemporalType(TemporalTypeEnumeration.TIMESTAMP);

		final DomainAttributeValidator validator = DomainFactory.eINSTANCE.createDomainAttributeValidator();
		validator.setNullable(column.isNullable());
		validator.setRegularExpression("");
		validator.setMinValue("");
		validator.setMaxValue("");

		// Exchange the attribute's type if it is mapped to a primitive type and the column can be null
		if (column.isNullable() && attr.getJavaType().isPrimitive())
			attr.setJavaType(project.getJavaTypeByName(attr.getJavaType().getWrapperTypeName()));

		if (column.getLength() > 0)
			validator.setMaxLength(column.getLength());

		attr.setDomainAttributeValidator(validator);

		// Add the domain attribute to the model
		revEngObj.addAttribute(attr, true);

		RevEngDomainAssociation reverseAssoc = null;

		if (mto.getTarget() != null)
			reverseAssoc = revEngAssoc.getReverseOneToManyAssoc();

		if (reverseAssoc != null) {
			// Remove the reverse association from the model
			final RevEngDomainObject reverseObj = reverseAssoc.getParentObject();
			reverseObj.getAssociations().remove(reverseAssoc);
		}

		revEngObj.getAssociations().remove(revEngAssoc);

		return true;
	}

}
