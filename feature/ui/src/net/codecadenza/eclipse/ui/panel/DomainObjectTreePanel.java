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
package net.codecadenza.eclipse.ui.panel;

import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;
import static net.codecadenza.eclipse.shared.Constants.IMG_ID_ATTRIBUTE;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTO_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_OTM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_OTO_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_UK_ATTRIBUTE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.form.init.util.AssociationHelper;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.TreeAdapter;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Panel that provides a tree view for a given domain object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectTreePanel extends Composite {
	private static final String ASSOCATION_KEY = "ASSOC";
	private static final String SELECT_KEY = "SELECT_NAME_KEY";

	private final Tree treeDomainObject;
	private final Mode mode;
	private boolean standardConversion;

	// Enumeration with supported operation modes
	public enum Mode {
		STANDARD, VIEW, GRID, TREE, EXCHANGE
	}

	/**
	 * Constructor
	 * @param parent
	 * @param mode
	 */
	public DomainObjectTreePanel(Composite parent, Mode mode) {
		super(parent, SWT.NONE);

		this.mode = mode;
		this.setLayout(new FillLayout());

		treeDomainObject = new Tree(this, SWT.BORDER);

		treeDomainObject.addTreeListener(new TreeAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.TreeAdapter#treeExpanded(org.eclipse.swt.events.TreeEvent)
			 */
			@Override
			public void treeExpanded(TreeEvent e) {
				final var item = (TreeItem) e.item;

				if (!(item.getData() instanceof final AbstractDomainAssociation assoc))
					return;

				if (mode == Mode.STANDARD) {
					item.removeAll();

					final boolean addAssociations = assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation;

					addDomainObjectItems(assoc.getDomainObject(), assoc.getTarget(), item, addAssociations);
				}
				else if (mode == Mode.EXCHANGE) {
					item.removeAll();

					addDomainObjectItemsForExchange(assoc.getTarget(), item);
				}
				else {
					final var parentAssocHelper = (AssociationHelper) item.getData(ASSOCATION_KEY);

					if (!parentAssocHelper.isLoaded()) {
						item.removeAll();

						if (mode == Mode.VIEW || mode == Mode.GRID)
							addDomainObjectItemsForView(assoc.getDomainObject(), assoc.getTarget(), item, parentAssocHelper);
						else if (mode == Mode.TREE)
							addDomainObjectItemsForTreeView(assoc.getDomainObject(), assoc.getTarget(), item, parentAssocHelper);

						parentAssocHelper.setLoaded(true);
					}
				}
			}
		});

		treeDomainObject.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				onMouseDoubleClick(getSelectedItem());
			}
		});

		// Allow items to be copied from the tree
		final var dragSourceDomainObject = new DragSource(treeDomainObject, DND.DROP_COPY);

		// Provide the data in text format
		dragSourceDomainObject.setTransfer(TextTransfer.getInstance());

		dragSourceDomainObject.addDragListener(new DragSourceAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragStart(DragSourceEvent event) {
				final TreeItem selItem = getSelectedItem();

				event.doit = false;

				if (selItem == null || selItem.getData() == null)
					return;

				if (selItem.getData() instanceof DomainAttribute)
					event.doit = true;

				if (mode == Mode.TREE
						&& (selItem.getData() instanceof ManyToManyAssociation || selItem.getData() instanceof OneToManyAssociation))
					event.doit = true;

				// In standard mode an association must not be dragged if standard conversion isn't set! A one-to-one association must not
				// be selected!
				if (mode == Mode.STANDARD && standardConversion && selItem.getData() instanceof AbstractDomainAssociation
						&& !(selItem.getData() instanceof OneToOneAssociation))
					event.doit = true;

				if (mode == Mode.EXCHANGE && selItem.getData() instanceof AbstractDomainAssociation)
					event.doit = true;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragSetData(DragSourceEvent event) {
				final TreeItem selItem = getSelectedItem();

				event.data = null;

				if (selItem == null || selItem.getData() == null)
					return;

				if (selItem.getData() instanceof DomainAttribute)
					event.data = selItem.getData().toString();

				if (mode == Mode.TREE
						&& (selItem.getData() instanceof ManyToManyAssociation || selItem.getData() instanceof OneToManyAssociation))
					event.data = selItem.getData().toString();

				if (mode == Mode.STANDARD && standardConversion && selItem.getData() instanceof AbstractDomainAssociation
						&& !(selItem.getData() instanceof OneToOneAssociation))
					event.data = selItem.getData().toString();

				if (mode == Mode.EXCHANGE && selItem.getData() instanceof AbstractDomainAssociation)
					event.data = selItem.getData().toString();
			}
		});
	}

	/**
	 * Constructor
	 * @param parent
	 */
	public DomainObjectTreePanel(Composite parent) {
		this(parent, Mode.STANDARD);
	}

	/**
	 * Set the internal flag that controls the tree view structure and the behaviour when dragging items
	 * @param standardConversion
	 */
	public void setStandardConversion(boolean standardConversion) {
		this.standardConversion = standardConversion;
	}

	/**
	 * Initialize the tree view by using the provided domain object
	 * @param domainObject
	 * @return the root association helper object
	 */
	public AssociationHelper init(DomainObject domainObject) {
		treeDomainObject.removeAll();

		final var rootAssociation = new AssociationHelper(AssociationHelper.INITIAL_ALIAS, 0, domainObject.getName(), null);

		final var itemDomainObject = new TreeItem(treeDomainObject, SWT.NONE);
		itemDomainObject.setText(domainObject.getName());
		itemDomainObject.setData(domainObject);
		itemDomainObject.setExpanded(true);
		itemDomainObject.setImage(JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CLASS));

		if (mode == Mode.VIEW || mode == Mode.GRID)
			addDomainObjectItemsForView(null, domainObject, itemDomainObject, rootAssociation);
		else if (mode == Mode.TREE)
			addDomainObjectItemsForTreeView(null, domainObject, itemDomainObject, rootAssociation);
		else if (mode == Mode.EXCHANGE)
			addDomainObjectItemsForExchange(domainObject, itemDomainObject);
		else
			addDomainObjectItems(null, domainObject, itemDomainObject, true);

		rootAssociation.setLoaded(true);

		return rootAssociation;
	}

	/**
	 * @return the selected tree item or null if no item is selected
	 */
	public TreeItem getSelectedItem() {
		return Arrays.asList(treeDomainObject.getSelection()).stream().findFirst().orElse(null);
	}

	/**
	 * Determine the list of all associations that are necessary to access a domain attribute by traversing from a given domain
	 * object to the domain attribute
	 * @return a list containing all associations
	 */
	public List<AbstractDomainAssociation> getAssociationListOfSelectedTreeItem() {
		final TreeItem selectedItem = getSelectedItem();
		final var assocList = new ArrayList<AbstractDomainAssociation>();

		if (selectedItem == null || selectedItem.getData() == null)
			return new ArrayList<>();

		TreeItem loopItem = selectedItem.getParentItem();

		// In standard mode it is allowed to drag items that are mapped to domain associations!
		if (mode == Mode.STANDARD && selectedItem.getData() instanceof AbstractDomainAssociation)
			loopItem = selectedItem;

		while (loopItem != null && loopItem.getData() != null
				&& loopItem.getData() instanceof final AbstractDomainAssociation assoc) {
			if ((mode == Mode.TREE || mode == Mode.EXCHANGE)
					&& (loopItem.getData() instanceof ManyToManyAssociation || loopItem.getData() instanceof OneToManyAssociation))
				break;

			assocList.add(assoc);

			loopItem = loopItem.getParentItem();
		}

		return assocList;
	}

	/**
	 * Determine if the given domain attribute should be added to the tree view. This method is intended to be overwritten!
	 * @param attr
	 * @return true if an item that refers to the given domain attribute should be added to the tree
	 */
	@SuppressWarnings("unused")
	protected boolean addDomainAttribute(DomainAttribute attr) {
		return true;
	}

	/**
	 * Determine if the given domain association should be added to the tree view. This method is intended to be overwritten!
	 * @param assoc
	 * @return true if an item that refers to the given domain association should be added to the tree
	 */
	@SuppressWarnings("unused")
	protected boolean addDomainAssociation(AbstractDomainAssociation assoc) {
		return true;
	}

	/**
	 * Callback listener for handling double-click events
	 * @param selectedItem
	 */
	@SuppressWarnings("unused")
	protected void onMouseDoubleClick(TreeItem selectedItem) {
		// The implementation must be provided by a subclass!
	}

	/**
	 * Add attributes and associations of the given domain object to the tree view
	 * @param parentDomainObject
	 * @param domainObject
	 * @param parentItem
	 * @param parentAssocHelper
	 */
	private void addDomainObjectItemsForView(DomainObject parentDomainObject, DomainObject domainObject, TreeItem parentItem,
			AssociationHelper parentAssocHelper) {
		try {
			addDomainAttributesToTree(domainObject, parentItem);

			// Add associations to the tree
			for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
				final AssociationHelper helper = initAssociationHelper(parentDomainObject, assoc, parentAssocHelper);

				if (helper == null)
					continue;

				parentAssocHelper.getChildren().add(helper);

				addDomainAssociationToTree(assoc, parentItem, helper, true);
			}

			parentItem.setExpanded(true);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Add attributes and associations of the given domain object to the tree view
	 * @param parentDomainObject
	 * @param domainObject
	 * @param parentItem
	 * @param parentAssocHelper
	 */
	private void addDomainObjectItemsForTreeView(DomainObject parentDomainObject, DomainObject domainObject, TreeItem parentItem,
			AssociationHelper parentAssocHelper) {
		try {
			addDomainAttributesToTree(domainObject, parentItem);

			// Add associations to the tree
			for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
				final AssociationHelper helper = initAssociationHelper(parentDomainObject, assoc, parentAssocHelper);

				if (helper == null)
					continue;

				// We must not add one-to-many and many-to-many associations that are not directly linked to the root via other to-many
				// associations!
				if (assoc instanceof OneToManyAssociation || assoc instanceof ManyToManyAssociation) {
					if (parentAssocHelper.getAssociation() instanceof ManyToOneAssociation)
						continue;

					if (parentAssocHelper.getAssociation() instanceof OneToOneAssociation)
						continue;
				}

				parentAssocHelper.getChildren().add(helper);

				addDomainAssociationToTree(assoc, parentItem, helper, true);
			}

			parentItem.setExpanded(true);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * Add attributes and associations of the given domain object to the tree view
	 * @param parentDomainObject
	 * @param domainObject
	 * @param parentItem
	 * @param addAssociations
	 */
	private void addDomainObjectItems(DomainObject parentDomainObject, DomainObject domainObject, TreeItem parentItem,
			boolean addAssociations) {
		boolean addAttributes = true;

		try {
			// Don't display attributes for target domain objects of many-to-many associations!
			if (parentItem.getData() instanceof ManyToManyAssociation)
				addAttributes = false;

			// Add attributes to tree
			if (addAttributes)
				addDomainAttributesToTree(domainObject, parentItem);

			// Add associations to tree
			if (addAssociations)
				for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
					// Do not add items if the association is bidirectional to the parent
					if (parentDomainObject != null && parentDomainObject.equals(assoc.getTarget()))
						continue;

					addDomainAssociationToTree(assoc, parentItem, null,
							assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation);
				}

			parentItem.setExpanded(true);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}
	}

	/**
	 * Add attributes and associations of the given domain object to the tree view
	 * @param domainObject
	 * @param parentItem
	 */
	private void addDomainObjectItemsForExchange(DomainObject domainObject, TreeItem parentItem) {
		try {
			// Add attributes to the tree
			addDomainAttributesToTree(domainObject, parentItem);

			// Add associations to the tree
			domainObject.getAllAssociations().forEach(assoc -> addDomainAssociationToTree(assoc, parentItem, null, true));

			parentItem.setExpanded(true);
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
		}
	}

	/**
	 * Add attributes of the given domain object to the tree view
	 * @param domainObject
	 * @param parentItem
	 */
	private void addDomainAttributesToTree(DomainObject domainObject, TreeItem parentItem) {
		final var attributes = new BasicEList<DomainAttribute>();
		attributes.addAll(domainObject.getAllAttributes());

		for (final DomainAttribute attr : attributes.stream().sorted(this::compareDomainAttributes).toList()) {
			if (!addDomainAttribute(attr))
				continue;

			var itemText = "";

			if (mode == Mode.VIEW || mode == Mode.GRID || mode == Mode.TREE) {
				if (attr.isLob() || !attr.isPersistent()
						|| (mode != Mode.TREE && attr.getCollectionType() != CollectionTypeEnumeration.NONE))
					continue;

				if (parentItem.getData() instanceof AbstractDomainAssociation) {
					final var assocHelper = (AssociationHelper) parentItem.getData(ASSOCATION_KEY);
					itemText = assocHelper.getAlias() + "." + attr.getName();
				}
				else
					itemText = AssociationHelper.INITIAL_ALIAS + "." + attr.getName();

				itemText += " (" + attr.getTypeName() + ")";
			}
			else {
				// Transient fields, byte arrays or element collections are not supported for search operations!
				if (mode == Mode.STANDARD && !standardConversion
						&& (!attr.isPersistent() || attr.isLob() || attr.getCollectionType() != CollectionTypeEnumeration.NONE))
					continue;

				itemText = attr.getTypeName() + " " + attr.getName();
			}

			final var item = new TreeItem(parentItem, SWT.NONE);
			item.setText(itemText);
			item.setData(attr);

			if (attr.isPk())
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ID_ATTRIBUTE));
			else if (attr.isDisplayAttribute())
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_UK_ATTRIBUTE));
			else
				item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));

			if (mode == Mode.TREE)
				if (parentItem.getData() instanceof AbstractDomainAssociation) {
					final var assocHelper = (AssociationHelper) parentItem.getData(ASSOCATION_KEY);
					item.setData(SELECT_KEY, assocHelper.getAlias() + "." + attr.getName());
				}
				else
					item.setData(SELECT_KEY, AssociationHelper.INITIAL_ALIAS + "." + attr.getName());
		}
	}

	/**
	 * Add the given domain association to the tree view
	 * @param assoc
	 * @param parentItem
	 * @param helper
	 * @param addDummyItem
	 */
	private void addDomainAssociationToTree(AbstractDomainAssociation assoc, TreeItem parentItem, AssociationHelper helper,
			boolean addDummyItem) {
		if (!addDomainAssociation(assoc))
			return;

		final var item = new TreeItem(parentItem, SWT.NONE);
		item.setData(assoc);

		if (assoc instanceof ManyToManyAssociation) {
			item.setText(
					JavaTypeModifierEnumeration.COLLECTION.toString() + "<" + assoc.getTarget().getName() + "> " + assoc.getName());
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_MTM_ASSOC));
		}
		if (assoc instanceof OneToManyAssociation) {
			item.setText(
					JavaTypeModifierEnumeration.COLLECTION.toString() + "<" + assoc.getTarget().getName() + "> " + assoc.getName());
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_OTM_ASSOC));
		}
		else if (assoc instanceof ManyToOneAssociation) {
			item.setText(assoc.getTarget().getName() + " " + assoc.getName());
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_MTO_ASSOC));
		}
		else if (assoc instanceof OneToOneAssociation) {
			item.setText(assoc.getTarget().getName() + " " + assoc.getName());
			item.setImage(CodeCadenzaResourcePlugin.getImage(IMG_OTO_ASSOC));
		}

		if (helper != null)
			item.setData(ASSOCATION_KEY, helper);

		if (addDummyItem) {
			final var dummyItem = new TreeItem(item, SWT.NONE);
			dummyItem.setText("");
		}
	}

	/**
	 * Compare the given domain attributes
	 * @param attr1
	 * @param attr2
	 * @return the result of the compare
	 */
	private int compareDomainAttributes(DomainAttribute attr1, DomainAttribute attr2) {
		int value1 = 0;
		int value2 = 0;

		if (attr1.isPk())
			value1 = 2;
		else if (attr1.isDisplayAttribute())
			value1 = 1;

		if (attr2.isPk())
			value2 = 2;
		else if (attr2.isDisplayAttribute())
			value2 = 1;

		return value2 - value1;
	}

	/**
	 * @param parentDomainObject
	 * @param assoc
	 * @param parentAssocHelper
	 * @return the initialized association helper or null if the association should not be added to the tree view
	 */
	private AssociationHelper initAssociationHelper(DomainObject parentDomainObject, AbstractDomainAssociation assoc,
			AssociationHelper parentAssocHelper) {
		boolean isOptional = false;

		// Do not add items if the association is bidirectional to the parent
		if (parentDomainObject != null && parentDomainObject.equals(assoc.getTarget()))
			return null;

		// Search for unsupported recursive structures. Return null if a recursive structure has been found!
		if (searchRecursiveStructure(assoc))
			return null;

		if (assoc instanceof OneToOneAssociation || assoc instanceof ManyToOneAssociation) {
			if (assoc instanceof final OneToOneAssociation oneToOne)
				isOptional = oneToOne.isOptional();
			else {
				final var manyToOne = (ManyToOneAssociation) assoc;
				isOptional = manyToOne.isOptional();
			}
		}
		else if (mode == Mode.TREE) {
			// A new hierarchy begins in case of one-to-many and many-to-many associations!
			return new AssociationHelper(AssociationHelper.INITIAL_ALIAS, 0, assoc.getName(), assoc, parentAssocHelper);
		}

		final var helper = new AssociationHelper(parentAssocHelper.getAlias(), parentAssocHelper.getCounter(), assoc.getName(), assoc,
				parentAssocHelper);
		helper.nextAlias();
		helper.checkAlias();
		helper.setOuterJoin(isOptional);

		return helper;
	}

	/**
	 * @param assoc
	 * @return true if the association references a domain object that in turn is either the same, a subclass or a parent class the
	 *         association belongs to
	 */
	private boolean searchRecursiveStructure(AbstractDomainAssociation assoc) {
		final DomainObject domainObject = assoc.getDomainObject();
		final Project project = domainObject.getNamespace().getProject();

		for (final DomainObject projectDomainObject : project.getAllDomainObjectsOfProject(false, true))
			if (projectDomainObject.getFullInheritanceTree().contains(domainObject))
				for (final DomainObject subDomainObject : projectDomainObject.getFullInheritanceTree())
					if (assoc.getTarget().equals(subDomainObject))
						return true;

		return false;
	}

}
