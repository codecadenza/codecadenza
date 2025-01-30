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
package net.codecadenza.eclipse.generator.client.common.tree;

import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDeleteMethod;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getEditForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getReadOnlyForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getSubItemRemoveMethod;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;

/**
 * <p>
 * Base class for all tree view generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractTreeViewGenerator extends AbstractJavaSourceGenerator {
	protected final TreeView tree;
	protected final Project project;
	protected final HashSet<String> menuSet = new HashSet<>();
	protected final HashSet<String> methodSet = new HashSet<>();
	protected final HashSet<String> menuMethodInitializerSet = new HashSet<>();
	protected final HashMap<TreeViewItem, DTOBean> subTreeItemDTOMap = new HashMap<>();
	protected final HashSet<DTOBean> distinctDTOSet = new HashSet<>();
	protected final HashMap<String, DomainObject> helperDTOMap = new HashMap<>();
	protected final ArrayList<TreeViewItem> dropItems = new ArrayList<>();
	protected final TreeViewItem rootItem;
	protected final boolean addAdvSearch;
	protected final boolean addQuickSearch;
	protected final boolean recursiveStructure;
	protected boolean needsDateTimeFormatter;
	protected boolean needsDateFormatter;
	protected boolean needsDecimalFormatter;
	protected boolean addSecurity;
	protected boolean saveQueries;

	/**
	 * Constructor
	 * @param tree
	 */
	protected AbstractTreeViewGenerator(TreeView tree) {
		super(tree.getSourceFile());

		this.tree = tree;
		this.rootItem = tree.getRootTreeItem();
		this.project = tree.getDomainObject().getNamespace().getProject();
		this.addAdvSearch = !tree.getAdvancedSearchItems().isEmpty();
		this.addQuickSearch = !tree.getQuickSearchItems().isEmpty();
		this.recursiveStructure = tree.getRecursiveMethod() != null;

		checkFormatters(rootItem);
	}

	/**
	 * Initialize internal fields by recursively traversing the entire tree view structure. All subclasses must call this method
	 * after calling super()!
	 */
	protected void initializeInternalFields() {
		if (rootItem.getDropMethod() != null
				&& (!addSecurity || rootItem.getDropMethod().getPermissionMode() != PermissionModeEnumeration.DENY_ALL))
			dropItems.add(rootItem);

		distinctDTOSet.add(rootItem.getItemDTO());

		initializeInternalFields(rootItem.getChildren());
	}

	/**
	 * Initialize internal fields based on the provided tree view items.
	 * @param items
	 */
	protected void initializeInternalFields(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			if (item.getDropMethod() != null
					&& (!addSecurity || item.getDropMethod().getPermissionMode() != PermissionModeEnumeration.DENY_ALL))
				dropItems.add(item);

			distinctDTOSet.add(item.getItemDTO());
			helperDTOMap.put(item.getAssociation().getUpperCaseName(), item.getItemDTO().getDomainObject());

			initializeInternalFields(item.getChildren());
		});
	}

	/**
	 * Add methods for fetching data. If a subclass wants to use this method it must override it!
	 * @param items
	 * @throws IllegalStateException if the subclass doesn't provide its own implementation
	 */
	protected void addItemFetchMethods(@SuppressWarnings("unused") Collection<TreeViewItem> items) {
		throw new IllegalStateException("Not implemented!");
	}

	/**
	 * Add methods for the initialization of parent context menus. If a subclass wants to use this method it must override it!
	 * @param items
	 * @throws IllegalStateException if the subclass doesn't provide its own implementation
	 */
	protected void addTreeParentMenuMethods(@SuppressWarnings("unused") Collection<TreeViewItem> items) {
		throw new IllegalStateException("Not implemented!");
	}

	/**
	 * Add specific methods for a given tree item. If a subclass wants to use this method it must override it!
	 * @param treeItem
	 * @throws IllegalStateException if the subclass doesn't provide its own implementation
	 */
	protected void addItemMethods(@SuppressWarnings("unused") TreeViewItem treeItem) {
		throw new IllegalStateException("Not implemented!");
	}

	/**
	 * Add the method for the initialization of the context menu for the given tree view item. If a subclass wants to use this
	 * method it must override it!
	 * @param treeItem
	 * @throws IllegalStateException if the subclass doesn't provide its own implementation
	 */
	protected void addItemMenuMethod(@SuppressWarnings("unused") TreeViewItem treeItem) {
		throw new IllegalStateException("Not implemented!");
	}

	/**
	 * Recursive method to add all sub-item data transfer objects
	 * @param items
	 */
	protected void addSubItemHelperDTOs(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			addHelperDTO(item);
			addSubItemHelperDTOs(item.getChildren());
		});
	}

	/**
	 * Recursive method to add all methods that are responsible for fetching data
	 * @param items
	 */
	protected void addAllSubItemFetchMethods(Collection<TreeViewItem> items) {
		addItemFetchMethods(items);

		items.forEach(item -> addAllSubItemFetchMethods(item.getChildren()));
	}

	/**
	 * Add all comparator classes
	 * @param item
	 */
	protected void addComparatorClasses(TreeViewItem item) {
		addComparatorClass(item);
		addSubItemComparatorClasses(item.getChildren());
	}

	/**
	 * Add comparator classes for all sub-items
	 * @param items
	 */
	protected void addSubItemComparatorClasses(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			addComparatorClass(item);
			addSubItemComparatorClasses(item.getChildren());
		});
	}

	/**
	 * Recursive method to add all DTO imports
	 * @param items
	 */
	protected void addSubItemDTOImports(Collection<TreeViewItem> items) {
		items.forEach(treeItem -> {
			importPackage(treeItem.getItemDTO().getNamespace().toString());

			addSubItemDTOImports(treeItem.getChildren());
		});
	}

	/**
	 * Recursive method to add all boundary imports
	 * @param items
	 */
	protected void addSubItemBoundaryImports(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			importPackage(item.getDataFetchMethod().getBoundaryBean().getNamespace().toString());

			if (item.getAssociation() instanceof OneToManyAssociation) {
				final BoundaryMethod deleteMethod = getDeleteMethod(item);

				if (deleteMethod != null)
					importPackage(deleteMethod.getBoundaryBean().getNamespace().toString());
			}
			else {
				final BoundaryMethod removeMethod = getSubItemRemoveMethod(item);

				if (removeMethod != null)
					importPackage(removeMethod.getBoundaryBean().getNamespace().toString());
			}

			addSubItemBoundaryImports(item.getChildren());
		});
	}

	/**
	 * Add all methods that provide specific functionality for all available tree view items
	 */
	protected void addTreeMethods() {
		addItemMethods(rootItem);

		rootItem.getChildren().forEach(this::addSubItemTreeMethods);
	}

	/**
	 * Add all methods that provide specific functionality for a given tree item and all its children
	 * @param item
	 */
	protected void addSubItemTreeMethods(TreeViewItem item) {
		addItemMethods(item);

		item.getChildren().forEach(this::addSubItemTreeMethods);
	}

	/**
	 * Add methods that initialize all context menus
	 */
	protected void addTreeMenuMethods() {
		addItemMenuMethod(rootItem);

		rootItem.getChildren().forEach(this::addSubItemMenuMethods);
	}

	/**
	 * Recursive method to add all sub-item context menu methods
	 * @param item
	 */
	protected void addSubItemMenuMethods(TreeViewItem item) {
		addItemMenuMethod(item);

		subTreeItemDTOMap.put(item, item.getItemDTO());

		item.getChildren().forEach(this::addSubItemMenuMethods);
	}

	/**
	 * Recursive method to add all context menu methods of parent tree view items
	 * @param items
	 */
	protected void addAllSubItemMenuMethods(Collection<TreeViewItem> items) {
		addTreeParentMenuMethods(items);

		items.forEach(item -> addAllSubItemMenuMethods(item.getChildren()));
	}

	/**
	 * Check if date and/or decimal formatters are basically needed
	 * @param item
	 */
	protected void checkFormatters(TreeViewItem item) {
		item.getDisplayAttributes().forEach(attr -> {
			final JavaType type = attr.getDomainAttribute().getJavaType();

			if (type.isTemporalType()) {
				if (type.isLocalDate() || attr.getDomainAttribute().getTemporalType() == TemporalTypeEnumeration.DATE)
					needsDateFormatter = true;
				else
					needsDateTimeFormatter = true;
			}
			else if (type.isDecimalNumber())
				needsDecimalFormatter = true;
		});

		item.getNodes().forEach(node -> {
			final JavaType type = node.getDTOAttribute().getDomainAttribute().getJavaType();

			if (type.isTemporalType()) {
				if (type.isLocalDate() || node.getDTOAttribute().getDomainAttribute().getTemporalType() == TemporalTypeEnumeration.DATE)
					needsDateFormatter = true;
				else
					needsDateTimeFormatter = true;
			}
			else if (type.isDecimalNumber())
				needsDecimalFormatter = true;
		});

		if (needsDateFormatter && needsDateTimeFormatter && needsDecimalFormatter)
			return;

		for (final TreeViewItem subItem : item.getChildren()) {
			if (needsDateFormatter && needsDateTimeFormatter && needsDecimalFormatter)
				return;

			checkFormatters(subItem);
		}
	}

	/**
	 * Create a private comparator class for the given tree view item
	 * @param item
	 * @throws IllegalStateException if the tree view item doesn't provide a display attribute
	 */
	protected void addComparatorClass(TreeViewItem item) {
		final var b = new StringBuilder();

		// Take the first display attribute for sorting
		final DTOBeanAttribute selAttribute = item.getDisplayAttributes().stream().findFirst().orElse(null);

		if (selAttribute == null)
			throw new IllegalStateException("A display attribute could not be found!");

		var compName = "";

		if (item.isRootItem())
			compName = item.getItemDTO().getDomainObject().getName() + "Comparator";
		else {
			final String assocName = item.getAssociation().getName();
			compName = assocName.substring(0, 1).toUpperCase() + assocName.substring(1) + "Comparator";
		}

		b.append("\n/**\n");
		b.append(" * Comparator class for objects of type {@link " + selAttribute.getDTOBean().getName() + "}\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private class " + compName + " implements Comparator<" + selAttribute.getDTOBean().getName() + ">\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public int compare(" + selAttribute.getDTOBean().getName() + " a, ");
		b.append(selAttribute.getDTOBean().getName() + " b)\n");
		b.append("{\n");

		final String getter = selAttribute.getGetterName();
		final boolean isPrimitive = selAttribute.getSearchType().isPrimitive();

		if (!isPrimitive) {
			b.append("if(a." + getter + " == null && b." + getter + " == null)\n");
			b.append("return 0;\n");
			b.append("else if(a." + getter + " != null && b." + getter + " == null)\n");
			b.append("return 1;\n");
			b.append("else if(a." + getter + " == null && b." + getter + " != null)\n");
			b.append("return -1;\n\n");
			b.append("return a." + getter + ".compareTo(b." + getter + ");\n");
		}
		else {
			b.append("if(a." + getter + " > b." + getter + ")\n");
			b.append("return 1;\n");
			b.append("else if(a." + getter + " < b." + getter + ")\n");
			b.append("return -1;\n\n");
			b.append("return 0;\n");
		}

		b.append("}\n");
		b.append("}\n\n");

		addSubClass(compName, b.toString());
	}

	/**
	 * Create a private helper DTO for a given tree view item
	 * @param item
	 */
	protected void addHelperDTO(TreeViewItem item) {
		final var b = new StringBuilder();
		final DomainObject parentDomainObject = item.getParentItem().getItemDTO().getDomainObject();
		final String pkParentTypeName = parentDomainObject.getPKAttribute().getJavaType().getName();
		final String assocName = item.getAssociation().getUpperCaseName();
		final var dtoName = assocName + "TreeHelperDTO";

		subTreeItemDTOMap.put(item, item.getItemDTO());

		b.append("\n");
		b.append("/**\n");
		b.append(" * Helper class for tree view sub-items\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private class " + dtoName + " extends AbstractSubItemHelperDTO\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("private " + pkParentTypeName + " parentId;\n\n");
		b.append("public " + dtoName + "(" + pkParentTypeName + " parentId)\n");
		b.append("{\n");
		b.append("this.parentId = parentId;\n");
		b.append("}\n\n");
		b.append("public " + pkParentTypeName + " getParentId()\n");
		b.append("{\n");
		b.append("return parentId;\n");
		b.append("}\n");
		b.append("}\n\n");

		addSubClass(dtoName, b.toString());
	}

	/**
	 * @return true if a key listener should be added
	 */
	protected boolean addKeyListener() {
		return hasDeleteMethod() || hasUpdateOrReadonlyForm();
	}

	/**
	 * Check if at least one form of type 'UPDATE' or 'READONLY' exists that can be used
	 * @return true if a respective form has been found
	 */
	protected boolean hasUpdateOrReadonlyForm() {
		return distinctDTOSet.stream().anyMatch(distinctDTO -> getEditForm(distinctDTO.getDomainObject()) != null
				|| getReadOnlyForm(distinctDTO.getDomainObject()) != null);
	}

	/**
	 * Check if there is at least one delete method that can be used
	 * @return true if a delete method has been found
	 */
	protected boolean hasDeleteMethod() {
		final BoundaryMethod deleteMethodOfRootItem = getDeleteMethod(rootItem);

		return deleteMethodOfRootItem != null || subTreeItemDTOMap.entrySet().stream()
				.anyMatch(entry -> getSubItemRemoveMethod(entry.getKey()) != null || getDeleteMethod(entry.getKey()) != null);
	}

}
