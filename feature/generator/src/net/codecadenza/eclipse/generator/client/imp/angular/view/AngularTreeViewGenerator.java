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
package net.codecadenza.eclipse.generator.client.imp.angular.view;

import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getAddForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getCreateNewForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDeleteMethod;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDownloadMethods;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getEditForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getReadOnlyForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getSubItemRemoveMethod;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.security.AngularSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularURLGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for tree views of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularTreeViewGenerator extends AbstractTypeScriptSourceGenerator {
	private final TreeView tree;
	private final DomainObject domainObject;
	private final Project project;
	private final AngularI18NGenerator i18n;
	private final AngularSecurityHelper securityHelper;
	private final TreeViewItem rootItem;
	private final boolean recursiveStructure;
	private final boolean addAdvSearch;
	private final boolean addQuickSearch;
	private final List<TreeViewItem> dropItems = new ArrayList<>();
	private final Map<TreeViewItem, Set<Form>> formActionsOfItem = new HashMap<>();
	private final Map<TreeViewItem, Set<Form>> formActionsOfParent = new HashMap<>();
	private final Map<TreeViewItem, Set<BoundaryMethod>> methodActionsOfItem = new HashMap<>();
	private final Map<TreeViewItem, Set<BoundaryMethod>> methodActionsOfParent = new HashMap<>();
	private final Set<TreeViewItem> treeItemsWithActions = new HashSet<>();
	private final Set<BoundaryMethod> actionMethods = new HashSet<>();
	private boolean addFileService;

	/**
	 * Constructor
	 * @param tree
	 */
	public AngularTreeViewGenerator(TreeView tree) {
		super(tree.getTypeScriptSourceFile(), tree.getTitle());

		this.tree = tree;
		this.domainObject = tree.getDomainObject();
		this.project = domainObject.getNamespace().getProject();
		this.i18n = new AngularI18NGenerator(project);
		this.securityHelper = new AngularSecurityHelper(project);
		this.rootItem = tree.getRootTreeItem();
		this.addAdvSearch = !tree.getAdvancedSearchItems().isEmpty();
		this.addQuickSearch = !tree.getQuickSearchItems().isEmpty();
		this.recursiveStructure = tree.getRecursiveMethod() != null;

		initInternalFields();

		initActions(rootItem);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		final var eventInterfaces = new ArrayList<String>();

		final var primengTypes = new ArrayList<String>();
		primengTypes.add("TreeNode");
		primengTypes.add("MenuItem");

		if (!dropItems.isEmpty()) {
			primengTypes.add("TreeDragDropService");
			eventInterfaces.add("TreeNodeDropEvent");
		}

		if (recursiveStructure || !rootItem.getChildren().isEmpty())
			eventInterfaces.add("TreeNodeExpandEvent");

		importTypes(Stream.of("Component", "OnInit", "ViewChild"), "@angular/core");
		importTypes(primengTypes.stream(), "primeng/api");
		importType("ContextMenu", "primeng/contextmenu");
		importType("AbstractTreeView", "../../common/components/tree-view/abstract-tree-view");
		importType(rootItem.getItemDTO().getName(), "../../domain/" + rootItem.getItemDTO().getName().toLowerCase() + ".interface");

		if (!eventInterfaces.isEmpty())
			importTypes(eventInterfaces.stream(), "primeng/tree");

		if (tree.needsSearchObject()) {
			importType("SearchInput", "../../common/model/search-input.model");

			if (addAdvSearch || addQuickSearch)
				importType("FieldTypeEnum", "../../common/model/field-type.enum");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		formatter.addLine("@Component({");
		formatter.increaseIndent();
		formatter.addLine("templateUrl: './" + tree.getName().toLowerCase() + ".html'" + (dropItems.isEmpty() ? "" : ","));

		if (!dropItems.isEmpty())
			formatter.addLine("providers: [TreeDragDropService]");

		formatter.decreaseIndent();
		formatter.addLine("})");
		formatter.addLine("export class " + tree.getName() + " extends AbstractTreeView implements OnInit {");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addService(tree.getDTO());

		addServiceOfSuperclass("MessageService", "messageService", "primeng/api");
		addServiceOfSuperclass("ConfirmationService", "confirmationService", "primeng/api");
		addServiceOfSuperclass("Router", "router", "@angular/router");
		addServiceOfSuperclass("FormatterService", "formatterService", "../../common/services/formatter.service");
		addServiceOfSuperclass("I18NService", "i18n", "../../common/services/i18n.service");

		if (!dropItems.isEmpty())
			addService("TreeDragDropService", "dragAndDropService", null);

		if (securityHelper.isSecurityEnabled()) {
			addService("AuthService", "authService", "../../common/services/auth.service");

			importType("RoleEnum ", "../../common/model/role.enum");
		}

		if (addFileService)
			addService("FileService", "fileService", "../../common/services/file.service");

		treeItemsWithActions.forEach(treeItem -> {
			final var methods = new HashSet<BoundaryMethod>();

			if (methodActionsOfParent.get(treeItem) != null)
				methods.addAll(methodActionsOfParent.get(treeItem));

			if (methodActionsOfItem.get(treeItem) != null)
				methods.addAll(methodActionsOfItem.get(treeItem));

			methods.forEach(method -> addService(method.getBoundaryBean().getDomainObject()));
		});

		addContextMenuFields();

		if (addAdvSearch) {
			addField("SearchInput", "searchInput!").create();
			addField(null, "showSearchInputDialog").withDefaultValue("false").create();
		}

		tree.getQuickSearchItems()
				.forEach(item -> addField(null, item.getDTOAttribute().getName() + "Filter").withDefaultValue("''").create());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addMethods(AngularContentFormatter formatter) {
		addInitMethod(formatter);

		if (addQuickSearch)
			addQuickSearchMethod(formatter);

		if (addAdvSearch) {
			addInitSearchInputMethod(formatter);
			addAdvancedSearchCallbackMethod(formatter);
			addCountMethod(formatter);
		}

		addTreeItemMethod(formatter, rootItem, false);

		if (recursiveStructure)
			addTreeItemMethod(formatter, rootItem, true);

		tree.getAllSubTreeItems().forEach(treeItem -> addTreeItemMethod(formatter, treeItem, false));

		addSearchMethod(formatter);
		addNodeExpandMethod(formatter);
		addNodeDropHandler(formatter);
		addContextMenuItemMethods(formatter);
		addShowContextMenuMethod(formatter);

		i18n.save();
	}

	/**
	 * Add the method to initialize the tree view
	 * @param formatter
	 */
	private void addInitMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Initialize the tree view");
		formatter.addLine("ngOnInit() {");
		formatter.increaseIndent();
		formatter.addLine("console.log('Init tree view');");

		addContextMenuItems(formatter);

		formatter.addBlankLine();

		if (addAdvSearch)
			formatter.addLine("this.initSearchInput();");

		if (addAdvSearch || addQuickSearch)
			formatter.addLine("this.displayNumberOfItemsInStatusField(0);");
		else
			formatter.addLine("this.performSearchOperation();");

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Create fields for all context menus
	 */
	private void addContextMenuFields() {
		final var contextMenus = new HashSet<String>();

		treeItemsWithActions.forEach(treeItem -> {
			if (hasAction(treeItem, false)) {
				final var contextMenuName = "contextMenu" + treeItem.getItemDTO().getDomainObject().getName();
				final var menuItemArrayName = "menuItems" + treeItem.getItemDTO().getDomainObject().getName();

				// Avoid creating fields for context menus twice!
				if (!contextMenus.contains(contextMenuName)) {
					addField("ContextMenu", contextMenuName + "!").asViewChild(contextMenuName).create();
					addField("MenuItem[]", menuItemArrayName).withDefaultValue("[]").create();

					contextMenus.add(contextMenuName);
				}
			}

			if (hasAction(treeItem, true)) {
				var contextMenuName = "contextMenu" + treeItem.getItemDTO().getName();
				var menuItemArrayName = "menuItems" + treeItem.getItemDTO().getName();

				if (treeItem.getAssociation() != null) {
					contextMenuName = "contextMenu" + treeItem.getAssociation().getUpperCaseName();
					menuItemArrayName = "menuItems" + treeItem.getAssociation().getUpperCaseName();
				}

				addField("ContextMenu", contextMenuName + "!").asViewChild(contextMenuName).create();
				addField("MenuItem[]", menuItemArrayName).withDefaultValue("[]").create();
			}
		});
	}

	/**
	 * @param treeItem
	 * @param ofParent
	 * @return true if at least one action for the given tree item exists
	 */
	private boolean hasAction(TreeViewItem treeItem, boolean ofParent) {
		if (ofParent) {
			return !((formActionsOfParent.get(treeItem) == null || formActionsOfParent.get(treeItem).isEmpty())
					&& (methodActionsOfParent.get(treeItem) == null || methodActionsOfParent.get(treeItem).isEmpty()));
		}

		return !((formActionsOfItem.get(treeItem) == null || formActionsOfItem.get(treeItem).isEmpty())
				&& (methodActionsOfItem.get(treeItem) == null || methodActionsOfItem.get(treeItem).isEmpty()));
	}

	/**
	 * Add all context menu items to the tree view
	 * @param formatter
	 */
	private void addContextMenuItems(AngularContentFormatter formatter) {
		treeItemsWithActions.forEach(treeItem -> {
			if (hasAction(treeItem, false)) {
				final var menuItemArrayName = "menuItems" + treeItem.getItemDTO().getDomainObject().getName();

				if (formActionsOfItem.get(treeItem) != null)
					formActionsOfItem.get(treeItem).forEach(form -> addContextMenuItem(formatter, menuItemArrayName, form));

				if (methodActionsOfItem.get(treeItem) != null)
					methodActionsOfItem.get(treeItem).forEach(method -> addContextMenuItem(formatter, menuItemArrayName, method, null));
			}

			if (hasAction(treeItem, true)) {
				final String menuItemArrayName;

				if (treeItem.getAssociation() != null)
					menuItemArrayName = "menuItems" + treeItem.getAssociation().getUpperCaseName();
				else
					menuItemArrayName = "menuItems" + treeItem.getItemDTO().getName();

				if (formActionsOfParent.get(treeItem) != null)
					formActionsOfParent.get(treeItem).forEach(form -> addContextMenuItem(formatter, menuItemArrayName, form));

				if (methodActionsOfParent.get(treeItem) != null)
					methodActionsOfParent.get(treeItem)
							.forEach(method -> addContextMenuItem(formatter, menuItemArrayName, method, treeItem));
			}
		});
	}

	/**
	 * Add a context menu item that opens a form
	 * @param formatter
	 * @param menuItemArrayName
	 * @param form
	 */
	private void addContextMenuItem(AngularContentFormatter formatter, String menuItemArrayName, Form form) {
		final var itemContent = new StringBuilder("this.addContextMenuItem(this." + menuItemArrayName + ", ");
		final var methodName = "open" + form.getName();

		if (form.getFormType() == FormTypeEnumeration.ADD)
			itemContent.append(i18n.getI18NMessage("action_add", "Add") + ", 'pi pi-plus', ");
		else if (form.getFormType() == FormTypeEnumeration.CREATE)
			itemContent.append(i18n.getI18NMessage("action_create", "Create") + ", 'pi pi-plus', ");
		else if (form.getFormType() == FormTypeEnumeration.READONLY)
			itemContent.append(i18n.getI18NMessage("action_view", "View") + ", 'pi pi-search', ");
		else if (form.getFormType() == FormTypeEnumeration.UPDATE)
			itemContent.append(i18n.getI18NMessage("action_edit", "Edit") + ", 'pi pi-pencil', ");

		itemContent.append("() => this." + methodName + "());");

		formatter.addBlankLine();

		securityHelper.wrapSecurityCode(formatter, form.getRoles(), itemContent.toString());
	}

	/**
	 * Add a context menu item that invokes a boundary method
	 * @param formatter
	 * @param menuItemArrayName
	 * @param method
	 * @param treeItem
	 */
	private void addContextMenuItem(AngularContentFormatter formatter, String menuItemArrayName, BoundaryMethod method,
			TreeViewItem treeItem) {
		final var itemContent = new StringBuilder();
		var methodName = "confirm" + method.getName().substring(0, 1).toUpperCase() + method.getName().substring(1);

		if (method.getMethodType() == BoundaryMethodTypeEnumeration.DELETE) {
			itemContent.append("this.addContextMenuItem(this." + menuItemArrayName + ", ");
			itemContent.append(i18n.getI18NMessage("action_delete", "Delete"));
			itemContent.append(", 'pi pi-times', ");
			itemContent.append("() => this." + methodName + "());");
		}
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION) {
			itemContent.append("this.addContextMenuItem(this." + menuItemArrayName + ", ");
			itemContent.append(i18n.getI18NMessage("action_remove", "Remove"));
			itemContent.append(", 'pi pi-times', ");
			itemContent.append("() => this." + methodName + "());");
		}
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD) {
			itemContent.append("this.addContextMenuItem(this." + menuItemArrayName + ", ");
			itemContent.append(i18n.getI18NMessage("action_download", "Download"));
			itemContent.append(", 'pi pi-download', ");
			itemContent.append("() => this." + method.getName() + "());");
		}
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_PARENT) {
			if (treeItem.getAssociation() != null)
				methodName = "add" + treeItem.getAssociation().getUpperCaseName();
			else
				methodName = "addSubItemsToTree";

			itemContent.append("this.addContextMenuItem(this." + menuItemArrayName + ", ");
			itemContent.append(i18n.getI18NMessage("action_refresh", "Refresh"));
			itemContent.append(", 'pi pi-refresh', ");
			itemContent.append("() => this." + methodName + "(this.selectedNode));");
		}

		if (itemContent.isEmpty())
			return;

		formatter.addBlankLine();

		if (method.getMethodType() != BoundaryMethodTypeEnumeration.FIND_BY_PARENT
				&& method.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
			securityHelper.wrapSecurityCode(formatter, method.getRoles(), itemContent.toString());
		else
			formatter.addLine(itemContent.toString());
	}

	/**
	 * Add the method for performing a quick-search operation
	 * @param formatter
	 */
	private void addQuickSearchMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Perform a search operation by using the provided input fields");
		formatter.addLine("performQuickSearch() {");
		formatter.increaseIndent();
		formatter.addLine("const searchInput = new SearchInput();");
		formatter.addLine("searchInput.maxResult = 100;");
		formatter.addBlankLine();

		tree.getQuickSearchItems().forEach(treeItem -> {
			final var searchField = new StringBuilder();
			searchField.append("const searchField = searchInput.addSearchField('");
			searchField.append(treeItem.getDTOAttribute().getName() + "', ");
			searchField.append(i18n.getI18N(treeItem.getDTOAttribute()) + ", ");
			searchField.append("FieldTypeEnum.STRING, 100);");

			formatter.addLine("if (this." + treeItem.getDTOAttribute().getName() + "Filter) {");
			formatter.increaseIndent();
			formatter.addLine(searchField.toString());
			formatter.addLine("searchField.filterCriteria = this." + treeItem.getDTOAttribute().getName() + "Filter + '%';");
			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		});

		formatter.addLine("this.performSearchOperation(searchInput);");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the callback method for performing an advanced search operation
	 * @param formatter
	 */
	private void addAdvancedSearchCallbackMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Callback listener that notifies this component to start a search operation");
		formatter.addLine("onPerformSearchOperation(searchInput: SearchInput) {");
		formatter.increaseIndent();
		formatter.addLine("this.searchInput = searchInput;");
		formatter.addLine("this.showSearchInputDialog = false;");
		formatter.addBlankLine();
		formatter.addLine("this.performSearchOperation(this.searchInput);");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method to perform a search operation
	 * @param formatter
	 */
	private void addSearchMethod(AngularContentFormatter formatter) {
		final var itemListName = rootItem.getItemDTO().getDomainObject().getLowerCaseName() + "Items";
		final var addItemMethodName = "add" + rootItem.getItemDTO().getDomainObject().getUpperCaseName() + "Items";
		final var methodParam = addAdvSearch || addQuickSearch ? "searchInput: SearchInput" : "";
		final var invocationParam = tree.needsSearchObject() ? "searchInputBackend" : "";

		if (addAdvSearch || addQuickSearch)
			formatter.addBlockComment("Send the search input object to the back-end and add the received objects to the tree view");
		else
			formatter.addBlockComment("Load all available items from the back-end and add the received objects to the tree view");

		formatter.addLine("performSearchOperation(" + methodParam + ") {");
		formatter.increaseIndent();

		if (tree.needsSearchObject()) {
			if (addAdvSearch || addQuickSearch)
				formatter.addLine("const searchInputBackend = searchInput.convert(this.formatterService.getLocale());");
			else
				formatter.addLine("const searchInputBackend = new SearchInput().convert(this.formatterService.getLocale());");

			formatter.addBlankLine();
		}

		formatter.addLine("this.displayLoadingMessageInStatusField();");
		formatter.addBlankLine();

		if (addAdvSearch || addQuickSearch) {
			formatter.addLine("console.log('Perform search operation by using: ' + JSON.stringify(searchInput));");
			formatter.addBlankLine();
		}

		final var invocation = new AngularServiceInvocationGenerator(tree.getBoundaryMethod()).createInvocation(invocationParam);

		var errorHandler = "error: error => this.displayError(error, ";
		errorHandler += i18n.getI18NMessage("msg_errordataload", "Error while loading data!") + ")";

		formatter.addLine(invocation + ".subscribe({");
		formatter.increaseIndent();
		formatter.addLine("next: " + itemListName + " => this." + addItemMethodName + "(" + itemListName + "),");
		formatter.addLine(errorHandler);
		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method for adding tree items
	 * @param formatter
	 * @param treeItem
	 * @param addRecursiveItems
	 */
	private void addTreeItemMethod(AngularContentFormatter formatter, TreeViewItem treeItem, boolean addRecursiveItems) {
		final DTOBean itemDTO = treeItem.getItemDTO();
		final String itemName = itemDTO.getDomainObject().getLowerCaseName();
		final var itemListName = itemDTO.getDomainObject().getLowerCaseName() + "Items";
		final var nodeName = itemDTO.getDomainObject().getLowerCaseName() + "Node";
		final var nodeTypeName = itemDTO.getName().toUpperCase() + "_TYPE";
		final String parentNodeName;

		formatter.addBlockComment("Add " + itemDTO.getDomainObject().getLabel() + " items to the tree view");

		if (treeItem.equals(rootItem) && !addRecursiveItems) {
			final var addItemMethodName = "add" + itemDTO.getDomainObject().getUpperCaseName() + "Items";
			parentNodeName = "this.rootNodes";

			formatter.addLine(addItemMethodName + "(" + itemListName + ": " + itemDTO.getName() + "[]) {");
			formatter.increaseIndent();
			formatter.addLine(parentNodeName + " = [];");
		}
		else {
			BoundaryMethod method = tree.getRecursiveMethod();
			var addItemMethodName = "addSubItemsToTree";
			DTOBean parentDTO = treeItem.getItemDTO();
			parentNodeName = "parentNode";

			if (treeItem.getAssociation() != null) {
				parentDTO = treeItem.getParentItem().getItemDTO();
				method = treeItem.getDataFetchMethod();
				addItemMethodName = "add" + treeItem.getAssociation().getUpperCaseName();
			}

			final JavaType pkType = parentDTO.getPKAttribute().getDomainAttribute().getJavaType();
			final var selectedObjectId = "parentNode.data." + parentDTO.getPKAttribute().getName();
			final String subItemLabel = treeItem.getItemDTO().getDomainObject().getLabel();
			final String parentItemLabel = parentDTO.getDomainObject().getLabel();

			formatter.addLine(addItemMethodName + "(" + parentNodeName + ": TreeNode | TreeNode[] | null) {");
			formatter.increaseIndent();
			formatter.addIfStatement("!" + parentNodeName + " || Array.isArray(" + parentNodeName + ")", "return;", true);
			formatter.addLine("const id = " + selectedObjectId + (pkType.isIntegerOrLong() ? ".toString()" : "") + ";");
			formatter.addBlankLine();
			formatter.addLine("console.log('Load " + subItemLabel + " sub-items of " + parentItemLabel + " ' + id);");
			formatter.addBlankLine();
			formatter.addLine(new AngularServiceInvocationGenerator(method).createInvocation("id") + ".subscribe({");
			formatter.increaseIndent();
			formatter.addLine("next: " + itemListName + " => {");
			formatter.increaseIndent();
			formatter.addLine(parentNodeName + ".children = [];");
		}

		formatter.addBlankLine();
		formatter.addLine(itemListName + ".forEach(" + itemName + " => {");
		formatter.increaseIndent();
		formatter.addLine("let itemLabel = '';");

		createDisplayAttributeLabel(formatter, treeItem);

		formatter.addBlankLine();
		formatter.addLine("const " + nodeName + ": TreeNode = {");
		formatter.increaseIndent();
		formatter.addLine("label: itemLabel,");
		formatter.addLine("type: '" + nodeTypeName + "',");
		formatter.addLine("droppable: false,");
		formatter.addLine("draggable: " + dropItems.contains(treeItem) + ",");
		formatter.addLine("expanded: false,");
		formatter.addLine("leaf: false,");
		formatter.addLine("data: " + itemName);
		formatter.decreaseIndent();
		formatter.addLine("};");
		formatter.addBlankLine();

		if (!treeItem.getNodes().isEmpty() || !treeItem.getChildren().isEmpty()) {
			formatter.addLine(nodeName + ".children = [];");
			formatter.addBlankLine();
		}

		treeItem.getNodes().forEach(subNode -> {
			final DTOBeanAttribute attr = subNode.getDTOAttribute();

			if (attr.getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE) {
				final var subNodeName = subNode.getDTOAttribute().getName() + "Node";
				final var transKey = attr.getDTOBean().getName() + "_" + attr.getName();
				final var itemListAccessor = attr.getDTOBean().getDomainObject().getLowerCaseName() + "." + attr.getName();
				final JavaType type = attr.getDomainAttribute().getJavaType();
				final TemporalTypeEnumeration temporalType = attr.getDomainAttribute().getTemporalType();
				final String label = i18n.getI18NMessage(transKey.toLowerCase(), subNode.getLabel());
				final String itemLabel;

				formatter.addLineComment("Add elements of attribute '" + attr.getName() + "'");
				formatter.addLine("const " + subNodeName + ": TreeNode = { label: " + label + " };");
				formatter.addLine(subNodeName + ".children = [];");
				formatter.addLine(nodeName + ".children.push(" + subNodeName + ");");
				formatter.addBlankLine();
				formatter.addLine("for (const element of " + itemListAccessor + ") {");
				formatter.increaseIndent();

				if (type.isDecimalNumber())
					itemLabel = "this.formatterService.formatNumber(element) ?? ''";
				else if (type.isTemporalType())
					if (type.isLocalDate() || temporalType == TemporalTypeEnumeration.DATE)
						itemLabel = "this.formatterService.formatDate(element) ?? ''";
					else
						itemLabel = "this.formatterService.formatDateTime(element) ?? ''";
				else if (type.isIntegerOrLong())
					itemLabel = "element.toString()";
				else
					itemLabel = "element";

				formatter.addLine(subNodeName + ".children.push( { label: " + itemLabel + " } );");
				formatter.decreaseIndent();
				formatter.addLine("}");
			}
			else {
				formatter.addLineComment("Add node for attribute '" + attr.getName() + "'");

				createItemLabel(formatter, attr, false, false, subNode.getLabel());

				formatter.addBlankLine();
				formatter.addLine(nodeName + ".children.push( { label: itemLabel } );");
			}

			formatter.addBlankLine();
		});

		if (!treeItem.getChildren().isEmpty())
			formatter.addLineComment("Add container nodes");

		treeItem.getChildren().forEach(subItem -> {
			final var subNodeName = subItem.getAssociation().getName() + "Node";
			final var subNodeTypeName = subItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";
			final var transKey = tree.getName() + "_" + subItem.getAssociation().getName();

			formatter.addLine("itemLabel = " + i18n.getI18NMessage(transKey.toLowerCase(), subItem.getLabel()) + ";");
			formatter.addBlankLine();
			formatter.addLine("const " + subNodeName + ": TreeNode = {");
			formatter.increaseIndent();
			formatter.addLine("label: itemLabel,");
			formatter.addLine("type: '" + subNodeTypeName + "',");
			formatter.addLine("droppable: " + dropItems.contains(subItem) + ",");
			formatter.addLine("draggable: false,");
			formatter.addLine("expanded: false,");
			formatter.addLine("leaf: false,");
			formatter.addLine("data: " + itemName);
			formatter.decreaseIndent();
			formatter.addLine("};");
			formatter.addBlankLine();
			formatter.addLine(nodeName + ".children.push(" + subNodeName + ");");
		});

		if (treeItem.equals(rootItem) && !addRecursiveItems)
			formatter.addLine(parentNodeName + ".push(" + nodeName + ");");
		else {
			if (!treeItem.getChildren().isEmpty())
				formatter.addBlankLine();

			formatter.addIfStatement(parentNodeName + ".children", parentNodeName + ".children.push(" + nodeName + ");", false);
		}

		// Add a container node for items of a recursive structure
		if (recursiveStructure && treeItem.equals(rootItem)) {
			final var subNodeName = rootItem.getItemDTO().getDomainObject().getLowerCaseName() + "ContainerNode";
			final var subNodeTypeName = rootItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";
			final String label = domainObject.getLabelPlural().substring(0, 1).toUpperCase()
					+ domainObject.getLabelPlural().substring(1);
			final var transKey = tree.getName() + "_" + domainObject.getName();

			formatter.addBlankLine();
			formatter.addLine("itemLabel = " + i18n.getI18NMessage(transKey.toLowerCase(), label) + ";");
			formatter.addBlankLine();
			formatter.addLine("const " + subNodeName + ": TreeNode = {");
			formatter.increaseIndent();
			formatter.addLine("label: itemLabel,");
			formatter.addLine("type: '" + subNodeTypeName + "',");
			formatter.addLine("droppable: " + dropItems.contains(rootItem) + ",");
			formatter.addLine("draggable: false,");
			formatter.addLine("expanded: false,");
			formatter.addLine("leaf: false,");
			formatter.addLine("data: " + itemName);
			formatter.decreaseIndent();
			formatter.addLine("};");
			formatter.addBlankLine();
			formatter.addLine(nodeName + ".children.push(" + subNodeName + ");");
		}

		formatter.decreaseIndent();
		formatter.addLine("});");

		if (treeItem.equals(rootItem) && !addRecursiveItems) {
			formatter.addBlankLine();
			formatter.addLine("this.displayNumberOfItemsInStatusField(" + itemListName + ".length);");
		}
		else {
			var errorHandler = "error: error => this.displayError(error, ";
			errorHandler += i18n.getI18NMessage("msg_errordataload", "Error while loading data!") + ")";

			formatter.decreaseIndent();
			formatter.addLine("},");
			formatter.addLine(errorHandler);
			formatter.decreaseIndent();
			formatter.addLine("});");
		}

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method that loads items dynamically when expanding the parent node
	 * @param formatter
	 */
	private void addNodeExpandMethod(AngularContentFormatter formatter) {
		if (rootItem.getChildren().isEmpty() && !recursiveStructure)
			return;

		boolean firstItem = true;

		formatter.addBlockComment("Load the sub-items from the back-end and add them to the respective node in the tree view");
		formatter.addLine("expandNode($event: TreeNodeExpandEvent) {");
		formatter.increaseIndent();

		for (final Map.Entry<TreeViewItem, Set<BoundaryMethod>> entry : methodActionsOfParent.entrySet()) {
			final TreeViewItem treeItem = entry.getKey();
			final BoundaryMethod method = entry.getValue().stream()
					.filter(m -> m.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_PARENT).findFirst().orElse(null);

			if (method == null)
				continue;

			final String addItemMethodName;
			final var nodeTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			if (treeItem.getAssociation() != null)
				addItemMethodName = "add" + treeItem.getAssociation().getUpperCaseName();
			else
				addItemMethodName = "addSubItemsToTree";

			formatter.addLine((firstItem ? "" : "} else ") + "if ($event.node.type === '" + nodeTypeName + "') {");
			formatter.increaseIndent();
			formatter.addLine("this." + addItemMethodName + "($event.node);");
			formatter.decreaseIndent();

			firstItem = false;
		}

		if (!firstItem)
			formatter.addLine("}");

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Create the method for handling tree node drop operations
	 * @param formatter
	 */
	private void addNodeDropHandler(AngularContentFormatter formatter) {
		if (dropItems.isEmpty())
			return;

		boolean firstItem = true;

		formatter.addBlockComment("Callback listener that is triggered as soon as a tree node has been dropped");
		formatter.addLine("handleNodeDropEvent($event: TreeNodeDropEvent) {");
		formatter.increaseIndent();
		formatter.addLine("const dragNode = $event.dragNode;");
		formatter.addLine("const dropNode = $event.dropNode;");
		formatter.addBlankLine();
		formatter.addIfStatement("!dragNode || !dropNode", "return;", true);

		for (final TreeViewItem item : dropItems) {
			final var nodeTypeName = item.getItemDTO().getName().toUpperCase() + "_TYPE";
			final BoundaryMethod dropMethod = item.getDropMethod();
			final JavaType dragPkType = item.getItemDTO().getDomainObject().getPKAttribute().getJavaType();
			var dragId = "dragNode.data." + item.getItemDTO().getPKAttribute().getName();
			var nodeParentTypeName = item.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";
			JavaType dropPKType = dragPkType;
			String dropId;
			String invocation;

			if (item.getAssociation() != null) {
				dropId = "dropNode.data." + item.getParentItem().getItemDTO().getPKAttribute().getName();
				dropPKType = item.getParentItem().getItemDTO().getPKAttribute().getDomainAttribute().getJavaType();
			}
			else {
				dropId = "dropNode.data." + rootItem.getItemDTO().getPKAttribute().getName();
				nodeParentTypeName = domainObject.getName().toUpperCase() + "_FOLDER_TYPE";
			}

			dropId += dropPKType.isIntegerOrLong() ? ".toString()" : "";
			dragId += dragPkType.isIntegerOrLong() ? ".toString()" : "";

			final var typeCheck = new StringBuilder();
			typeCheck.append((firstItem ? "" : "} else "));
			typeCheck.append("if (dragNode.type === '" + nodeTypeName + "' ");
			typeCheck.append("&& dropNode.type === '" + nodeParentTypeName + "') {");

			formatter.addLine(typeCheck.toString());
			formatter.increaseIndent();

			if (item.getAssociation() == null) {
				formatter.addLineComment("Avoid cyclic reference!");
				formatter.addLine("if (dragNode.data !== dropNode.data) {");
				formatter.increaseIndent();
			}

			formatter.increaseIndent();

			if (item.getAssociation() == null || item.getAssociation() instanceof OneToManyAssociation)
				invocation = new AngularServiceInvocationGenerator(dropMethod).createInvocation(dragId, dropId);
			else
				invocation = new AngularServiceInvocationGenerator(dropMethod).createInvocation(dropId, dragId);

			invocation += ".subscribe({\n";
			invocation += formatter.getIndent();
			invocation += "error: error => this.displayError(error, "
					+ i18n.getI18NMessage("msg_errordropnode", "Drop operation failed!") + ")";

			formatter.decreaseIndent();

			if (securityHelper.isSecurityEnabled() && dropMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				securityHelper.wrapSecurityCode(formatter, dropMethod.getRoles(), invocation);
			else
				formatter.addLine(invocation);

			formatter.addLine("});");

			if (item.getAssociation() == null) {
				formatter.decreaseIndent();
				formatter.addLine("}");
			}

			formatter.decreaseIndent();
			firstItem = false;
		}

		if (!firstItem)
			formatter.addLine("}");

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Create the label of a tree node that represents the display attribute
	 * @param formatter
	 * @param treeItem
	 */
	private void createDisplayAttributeLabel(AngularContentFormatter formatter, TreeViewItem treeItem) {
		final int numberOfDisplayAttributes = treeItem.getDisplayAttributes().size();
		boolean isFirstAttribute = true;
		int attributeIndex = 0;

		for (final DTOBeanAttribute attr : treeItem.getDisplayAttributes()) {
			createItemLabel(formatter, attr, isFirstAttribute, true, null);

			if (numberOfDisplayAttributes > 1) {
				// The text for all subsequent attributes should be surrounded by parentheses!
				if (isFirstAttribute)
					formatter.addLine("itemLabel += ' (';");

				// Add a whitespace character before adding the text of subsequent attributes
				if (!isFirstAttribute && (attributeIndex + 1) < numberOfDisplayAttributes)
					formatter.addLine("itemLabel += ' ';");
			}

			attributeIndex++;
			isFirstAttribute = false;
		}

		if (numberOfDisplayAttributes > 1)
			formatter.addLine("itemLabel += ')';");
	}

	/**
	 * Create the label for a tree item's DTO attribute
	 * @param formatter
	 * @param attr
	 * @param skipLabel
	 * @param append
	 * @param label
	 */
	private void createItemLabel(AngularContentFormatter formatter, DTOBeanAttribute attr, boolean skipLabel, boolean append,
			String label) {
		final JavaType type = attr.getDomainAttribute().getJavaType();
		final TemporalTypeEnumeration temporalType = attr.getDomainAttribute().getTemporalType();
		final var itemValueAccessor = attr.getDTOBean().getDomainObject().getLowerCaseName() + "." + attr.getName();

		if (!skipLabel) {
			final var key = attr.getDTOBean().getName() + "_" + attr.getName();

			if (label == null || label.isEmpty())
				label = attr.getDomainAttribute().getLabel().substring(0, 1).toUpperCase()
						+ attr.getDomainAttribute().getLabel().substring(1);

			formatter.addLine("itemLabel " + (append ? "+" : "") + "= " + i18n.getI18NMessage(key.toLowerCase(), label) + " + ': ';");
		}

		if (type.isDecimalNumber())
			formatter.addLine("itemLabel += this.formatterService.formatNumber(" + itemValueAccessor + ");");
		else if (type.isTemporalType()) {
			if (type.isLocalDate() || temporalType == TemporalTypeEnumeration.DATE)
				formatter.addLine("itemLabel += this.formatterService.formatDate(" + itemValueAccessor + ");");
			else
				formatter.addLine("itemLabel += this.formatterService.formatDateTime(" + itemValueAccessor + ");");
		}
		else if (type.isEnum()) {
			final var transKey = "'" + type.getName().toLowerCase() + "_'";
			formatter
					.addLine("itemLabel += this.i18n.translate(" + transKey + " + " + itemValueAccessor + ".toString().toLowerCase());");
		}
		else
			formatter.addLine("itemLabel += " + itemValueAccessor + ";");
	}

	/**
	 * Add the method to initialize the search input object for an advanced search operation
	 * @param formatter
	 */
	private void addInitSearchInputMethod(AngularContentFormatter formatter) {
		formatter.addBlockComment("Define the initial search input object");
		formatter.addLine("initSearchInput(): SearchInput {");
		formatter.increaseIndent();
		formatter.addLine("this.searchInput = new SearchInput();");
		formatter.addLine("this.searchInput.maxResult = 100;");

		tree.getAdvancedSearchItems().forEach(treeItem -> {
			final DTOBeanAttribute dtoAttr = treeItem.getDTOAttribute();
			final DomainAttribute domainAttr = dtoAttr.getDomainAttribute();
			final JavaType type = domainAttr.getJavaType();
			final var transKey = dtoAttr.getDTOBean().getName() + "_asi_" + dtoAttr.getName();
			final var columnDef = new StringBuilder();
			final var fieldName = "field" + dtoAttr.getUpperCaseName();
			boolean resetDateTimeFormatFlag = false;

			if (type.isLocalDate() || (type.isDateOrCalendar() && domainAttr.getTemporalType() == TemporalTypeEnumeration.DATE))
				resetDateTimeFormatFlag = true;

			if (resetDateTimeFormatFlag || type.isEnum())
				columnDef.append(formatter.getIndent() + "const " + fieldName + " = this.searchInput.addSearchField(");
			else
				columnDef.append(formatter.getIndent() + "this.searchInput.addSearchField(");

			columnDef.append("'" + dtoAttr.getName() + "', ");
			columnDef.append(i18n.getI18NMessage(transKey.toLowerCase(), treeItem.getLabel()) + ", FieldTypeEnum.");

			if (type.isTemporalType())
				columnDef.append("DATE");
			else if (type.isBoolean())
				columnDef.append("BOOLEAN");
			else if (type.isIntegerOrLong())
				columnDef.append("INTEGER");
			else if (type.isDecimalNumber())
				columnDef.append("DECIMAL");
			else if (type.isString() || type.isChar())
				columnDef.append("STRING");
			else if (type.isUUID()) {
				if (domainAttr.isWildcardFilteringSupported())
					columnDef.append("UUID");
				else
					columnDef.append("UUID_BINARY");
			}
			else
				columnDef.append("ENUM");

			columnDef.append(", 100);\n");

			if (type.isEnum()) {
				final var javaEnum = (JavaEnum) type;

				addDependentEnum(javaEnum);

				columnDef.append(formatter.getIndent() + fieldName + ".addSelectionItem('');\n");

				javaEnum.getEnumerationValues().forEach(enumLiteral -> {
					columnDef.append(formatter.getIndent() + fieldName + ".addSelectionItem(");
					columnDef.append(javaEnum.getName() + "[" + javaEnum.getName() + "." + enumLiteral.getName() + "], ");
					columnDef.append(i18n.getI18N(enumLiteral) + ");\n");
				});

				importType(javaEnum.getName(), "../../domain/" + javaEnum.getName().toLowerCase() + ".enum");
			}

			if (resetDateTimeFormatFlag)
				columnDef.append(formatter.getIndent() + fieldName + ".dateTimeFormat = false;\n");

			formatter.addBlankLine();
			formatter.addContent(columnDef.toString());
		});

		formatter.addBlankLine();
		formatter.addLine("return this.searchInput;");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method to perform a count operation
	 * @param formatter
	 */
	private void addCountMethod(AngularContentFormatter formatter) {
		final String messageSummary = i18n.getI18NMessage("msg_countresult", "This query would return $(result) items!",
				"countResult.toString()");
		final String countMethodInvocation = new AngularServiceInvocationGenerator(tree.getCountMethod())
				.createInvocation("searchInputBackend");
		final String errorMsg = i18n.getI18NMessage("msg_errorcount", "Error while performing count operation!");

		formatter.addBlockComment("Callback listener that notifies this component to start a count operation");
		formatter.addLine("onPerformCountOperation(searchInput: SearchInput) {");
		formatter.increaseIndent();
		formatter.addLine("this.searchInput = searchInput;");
		formatter.addLine("this.showSearchInputDialog = false;");
		formatter.addLine("const searchInputBackend = this.searchInput.convert(this.formatterService.getLocale());");
		formatter.addBlankLine();
		formatter.addLine("console.log('Perform count operation by using: ' + JSON.stringify(this.searchInput));");
		formatter.addBlankLine();
		formatter.addLine(countMethodInvocation + ".subscribe({");
		formatter.increaseIndent();
		formatter.addLine("next: countResult => this.messageService.add({ severity: 'info', summary: " + messageSummary + " }),");
		formatter.addLine("error: error => this.displayError(error, " + errorMsg + ")");
		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add methods for all context menu items
	 * @param formatter
	 */
	private void addContextMenuItemMethods(AngularContentFormatter formatter) {
		final var forms = new HashSet<Form>();

		treeItemsWithActions.forEach(treeItem -> {
			if (formActionsOfItem.get(treeItem) != null)
				for (final Form form : formActionsOfItem.get(treeItem))
					if (!forms.contains(form)) {
						addOpenFormMethod(formatter, treeItem.getItemDTO(), form);
						forms.add(form);
					}

			if (formActionsOfParent.get(treeItem) != null)
				for (final Form form : formActionsOfParent.get(treeItem))
					if (!forms.contains(form)) {
						addOpenFormMethod(formatter,
								treeItem.getParentItem() != null ? treeItem.getParentItem().getItemDTO() : treeItem.getItemDTO(), form);
						forms.add(form);
					}

			if (methodActionsOfItem.get(treeItem) != null)
				methodActionsOfItem.get(treeItem).forEach(method -> addActionMethod(formatter, treeItem, method));

			if (methodActionsOfParent.get(treeItem) != null)
				methodActionsOfParent.get(treeItem).forEach(method -> addActionMethod(formatter, treeItem, method));
		});
	}

	/**
	 * Add the method to execute the given boundary method
	 * @param formatter
	 * @param treeItem
	 * @param method
	 */
	private void addActionMethod(AngularContentFormatter formatter, TreeViewItem treeItem, BoundaryMethod method) {
		final BoundaryMethodTypeEnumeration methodType = method.getMethodType();
		final DTOBean itemDTO = treeItem.getItemDTO();
		final DTOBeanAttribute pkAttr = itemDTO.getPKAttribute();
		var param = "this.selectedNode.data." + pkAttr.getName();

		if (methodType == BoundaryMethodTypeEnumeration.DELETE
				|| methodType == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION) {
			final var methodName = "confirm" + method.getName().substring(0, 1).toUpperCase() + method.getName().substring(1);

			formatter.addBlockComment("Open a confirmation dialog before deleting the selected item");
			formatter.addLine(methodName + "() {");
			formatter.increaseIndent();
			formatter.addLine("this.openConfirmDeleteDialog(this." + method.getName() + ".bind(this));");
			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		}
		else if (methodType == BoundaryMethodTypeEnumeration.DOWNLOAD) {
			final String errorMsg = i18n.getI18NMessage("msg_errordownload", "Error while performing file download operation!");
			var idParam = "selectedNode.data." + pkAttr.getName();
			var fileNameParam = "";

			for (final DTOBeanAttribute attr : treeItem.getItemDTO().getAttributes())
				if (attr.getDomainAttribute() != null && attr.getDomainAttribute().getTag() == AttributeTagEnumeration.DOCUMENT_NAME) {
					fileNameParam = ", selectedNode.data." + attr.getName();
					break;
				}

			importType("mergeMap", "rxjs/operators");

			if (pkAttr.getDomainAttribute().getJavaType().isIntegerOrLong())
				idParam += ".toString()";

			formatter.addBlockComment("Download file");
			formatter.addLine(method.getName() + "() {");
			formatter.increaseIndent();
			formatter.addLine("const selectedNode = this.selectedNode;");
			formatter.addBlankLine();
			formatter.addIfStatement("!selectedNode || Array.isArray(selectedNode)", "return;", true);
			formatter.addLineComment("Determine the real path of the file in the back-end");
			formatter.addLine(new AngularServiceInvocationGenerator(method).createInvocation(idParam) + ".pipe(");
			formatter.increaseIndent();
			formatter.addLine("mergeMap((path: string) => this.fileService.downloadFile(path)))");
			formatter.decreaseIndent();
			formatter.addLine(".subscribe({");
			formatter.increaseIndent();
			formatter.addLine("next: data => this.fileService.openFile(data" + fileNameParam + "),");
			formatter.addLine("error: error => this.displayError(error, " + errorMsg + ")");
			formatter.decreaseIndent();
			formatter.addLine("});");
			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		}

		if (methodType == BoundaryMethodTypeEnumeration.DELETE) {
			final String errorMsg = i18n.getI18NMessage("msg_errordelete", "Error while deleting object!");

			if (pkAttr.getDomainAttribute().getJavaType().isIntegerOrLong())
				param += ".toString()";

			formatter.addBlockComment("Delete the selected " + itemDTO.getDomainObject().getLabel());
			formatter.addLine(method.getName() + "() {");
			formatter.increaseIndent();
			formatter.addIfStatement("!this.selectedNode || Array.isArray(this.selectedNode)", "return;", true);
			formatter.addLine(new AngularServiceInvocationGenerator(method).createInvocation(param) + ".subscribe({");
			formatter.increaseIndent();
			formatter.addLine("error: error => this.displayError(error, " + errorMsg + "),");
			formatter.addLine("complete: () => this.removeNode(this.selectedNode)");
			formatter.decreaseIndent();
			formatter.addLine("});");
			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		}
		else if (methodType == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION) {
			final String errorMsg = i18n.getI18NMessage("msg_errordelete", "Error while deleting object!");
			final DTOBean parentDTO = treeItem.getParentItem().getItemDTO();
			final JavaType pkParentType = parentDTO.getPKAttribute().getDomainAttribute().getJavaType();
			final var nodeCheck = "!this.selectedNode || Array.isArray(this.selectedNode) || !this.selectedNode.parent";
			var parentId = "const parentId = this.selectedNode.parent.data." + parentDTO.getPKAttribute().getName();
			var itemId = "const id = this.selectedNode.data." + pkAttr.getName();

			if (pkParentType.isIntegerOrLong())
				parentId += ".toString()";

			parentId += ";";

			if (pkAttr.getDomainAttribute().getJavaType().isIntegerOrLong())
				itemId += ".toString()";

			itemId += ";";

			formatter.addBlockComment("Remove the selected " + itemDTO.getDomainObject().getLabel() + " from the respective list");
			formatter.addLine(method.getName() + "() {");
			formatter.increaseIndent();
			formatter.addIfStatement(nodeCheck, "return;", true);
			formatter.addLine(parentId);
			formatter.addLine(itemId);
			formatter.addBlankLine();
			formatter.addLine(new AngularServiceInvocationGenerator(method).createInvocation("parentId, id") + ".subscribe({");
			formatter.increaseIndent();
			formatter.addLine("error: error => this.displayError(error, " + errorMsg + "),");
			formatter.addLine("complete: () => this.removeNode(this.selectedNode)");
			formatter.decreaseIndent();
			formatter.addLine("});");
			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		}
	}

	/**
	 * Add the method to open a form
	 * @param formatter
	 * @param dto
	 * @param form
	 */
	private void addOpenFormMethod(AngularContentFormatter formatter, DTOBean dto, Form form) {
		final String targetFormURL = AngularURLGenerator.createURL(form, false);
		final var methodName = "open" + form.getName();
		var selectedItemId = "this.selectedNode.data." + dto.getPKAttribute().getName();

		if (dto.getPKAttribute().getDomainAttribute().getJavaType().isIntegerOrLong())
			selectedItemId += ".toString()";

		formatter.addBlockComment("Open form '" + form.getName() + "'");
		formatter.addLine(methodName + "() {");
		formatter.increaseIndent();

		if (form.getFormType() != FormTypeEnumeration.CREATE) {
			formatter.addIfStatement("!this.selectedNode || Array.isArray(this.selectedNode)", "return;", true);
			formatter.addLine("this.router.navigate(['" + targetFormURL + "/' + " + selectedItemId + "]);");
		}
		else
			formatter.addLine("this.router.navigate(['" + targetFormURL + "']);");

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add the method for opening the context menu depending on the selected node type
	 * @param formatter
	 */
	private void addShowContextMenuMethod(AngularContentFormatter formatter) {
		boolean firstItem = true;

		formatter.addBlockComment("Open the respective context menu for the given tree node");
		formatter.addLine("showContextMenu($event: MouseEvent, node: TreeNode) {");
		formatter.increaseIndent();

		for (final TreeViewItem treeItem : treeItemsWithActions) {
			if (hasAction(treeItem, false)) {
				final var contextMenuName = "contextMenu" + treeItem.getItemDTO().getDomainObject().getName();
				final var nodeTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";

				formatter.addLine((firstItem ? "" : "} else ") + "if (node.type === '" + nodeTypeName + "') {");
				formatter.increaseIndent();
				formatter.addLine("this.selectedNode = node;");
				formatter.addLine("this." + contextMenuName + ".show($event);");
				formatter.decreaseIndent();

				firstItem = false;
			}

			if (hasAction(treeItem, true)) {
				final var nodeTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";
				var contextMenuName = "contextMenu" + treeItem.getItemDTO().getName();

				if (treeItem.getAssociation() != null)
					contextMenuName = "contextMenu" + treeItem.getAssociation().getUpperCaseName();

				formatter.addLine((firstItem ? "" : "} else ") + "if (node.type === '" + nodeTypeName + "') {");
				formatter.increaseIndent();
				formatter.addLine("this.selectedNode = node;");
				formatter.addLine("this." + contextMenuName + ".show($event);");
				formatter.decreaseIndent();

				firstItem = false;
			}
		}

		if (!firstItem)
			formatter.addLine("}");

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Initialize actions for all tree items
	 * @param treeItem
	 */
	private void initActions(TreeViewItem treeItem) {
		BoundaryMethod method = null;

		addFormAction(treeItem, getEditForm(treeItem), false);
		addFormAction(treeItem, getReadOnlyForm(treeItem), false);

		if (treeItem.isRootItem())
			addFormAction(treeItem, getCreateNewForm(treeItem), false);

		addFormAction(treeItem, getAddForm(treeItem), true);

		getDownloadMethods(treeItem).forEach(downloadMethod -> addMethodAction(treeItem, downloadMethod, false));

		if (!treeItem.isRootItem()) {
			addMethodAction(treeItem, treeItem.getDataFetchMethod(), true);

			if (treeItem.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
				method = getDeleteMethod(treeItem);
			else
				method = getSubItemRemoveMethod(treeItem);
		}
		else {
			method = getDeleteMethod(treeItem);

			if (tree.getRecursiveMethod() != null)
				addMethodAction(treeItem, tree.getRecursiveMethod(), true);
		}

		addMethodAction(treeItem, method, false);

		treeItem.getChildren().forEach(this::initActions);
	}

	/**
	 * Add a form action for the given tree item to the internal data structure
	 * @param treeItem
	 * @param form
	 * @param addToParent
	 */
	private void addFormAction(TreeViewItem treeItem, Form form, boolean addToParent) {
		if (form == null)
			return;

		treeItemsWithActions.add(treeItem);

		if (addToParent) {
			formActionsOfParent.computeIfAbsent(treeItem, _ -> new HashSet<>());
			formActionsOfParent.get(treeItem).add(form);
		}
		else {
			formActionsOfItem.computeIfAbsent(treeItem, _ -> new HashSet<>());
			formActionsOfItem.get(treeItem).add(form);
		}
	}

	/**
	 * Add a method action for the given tree item to the internal data structure
	 * @param treeItem
	 * @param method
	 * @param addToParent
	 */
	private void addMethodAction(TreeViewItem treeItem, BoundaryMethod method, boolean addToParent) {
		if (method == null)
			return;

		final BoundaryMethodTypeEnumeration methodType = method.getMethodType();

		treeItemsWithActions.add(treeItem);
		actionMethods.add(method);

		if (methodType == BoundaryMethodTypeEnumeration.DOWNLOAD)
			addFileService = true;

		if (addToParent) {
			methodActionsOfParent.computeIfAbsent(treeItem, _ -> new HashSet<>());
			methodActionsOfParent.get(treeItem).add(method);
		}
		else {
			methodActionsOfItem.computeIfAbsent(treeItem, _ -> new HashSet<>());
			methodActionsOfItem.get(treeItem).add(method);
		}
	}

	/**
	 * Initialize internal fields
	 */
	private void initInternalFields() {
		final var allTreeItems = new ArrayList<TreeViewItem>();
		allTreeItems.add(rootItem);
		allTreeItems.addAll(tree.getAllSubTreeItems());

		allTreeItems.forEach(treeItem -> {
			if (treeItem.getDropMethod() != null && (!securityHelper.isSecurityEnabled()
					|| treeItem.getDropMethod().getPermissionMode() != PermissionModeEnumeration.DENY_ALL))
				dropItems.add(treeItem);

			addDependentDTO(treeItem.getItemDTO());

			for (final DTOBeanAttribute attr : treeItem.getItemDTO().getAttributes()) {
				if (attr.getDomainAttribute() == null)
					continue;

				final JavaType type = attr.getDomainAttribute().getJavaType();

				if (type.isEnum())
					addDependentEnum((JavaEnum) type);
			}
		});
	}

	/**
	 * Create the tree view template file
	 * @throws Exception if an internal error has occurred
	 */
	public void createTemplateFile() throws Exception {
		final var formatter = new AngularContentFormatter();

		final var allTreeItems = new ArrayList<TreeViewItem>();
		allTreeItems.add(rootItem);
		allTreeItems.addAll(tree.getAllSubTreeItems());

		final var container = new StringBuilder();
		container.append("<cc-view-container i18n-headerText=\"@@" + tree.getName().toLowerCase() + "_title\" ");
		container.append("headerText=\"" + tree.getTitle() + "\" headerIcon=\"pi-sitemap\">");

		formatter.addLine(container.toString());
		formatter.addBlankLine();
		formatter.increaseIndent();

		if (addAdvSearch) {
			formatter.addLine("<cc-search-input-dialog (closeDialog)=\"showSearchInputDialog = false\" ");
			formatter.increaseIndent();
			formatter.addLine("[visible]=\"showSearchInputDialog\"");
			formatter.addLine("[searchInput]=\"searchInput\" (performSearch)=\"onPerformSearchOperation($event)\" ");
			formatter.addLine("(performCount)=\"onPerformCountOperation($event)\">");
			formatter.decreaseIndent();
			formatter.addLine("</cc-search-input-dialog>");
			formatter.addBlankLine();
		}

		if (addQuickSearch) {
			formatter.addLine("<div class=\"form-field-table\">");
			formatter.increaseIndent();

			tree.getQuickSearchItems().forEach(searchItem -> {
				final DTOBeanAttribute dtoAttr = searchItem.getDTOAttribute();
				final var fieldName = dtoAttr.getName() + "Filter";
				final var transKey = dtoAttr.getDTOBean().getName() + "_qsi_" + dtoAttr.getName();

				var inputField = "<input pInputText [(ngModel)]=\"" + fieldName;
				inputField += "\" size=\"50\" (keyup.enter)=\"performQuickSearch()\" type=\"text\">";

				formatter.addLine("<div class=\"form-field-row\">");
				formatter.increaseIndent();
				formatter.addLine("<div class=\"form-field-cell label-field-mandatory\" i18n=\"@@" + transKey.toLowerCase() + "\">");
				formatter.increaseIndent();
				formatter.addLine(searchItem.getLabel() + ":");
				formatter.decreaseIndent();
				formatter.addLine("</div>");
				formatter.addLine("<div class=\"form-field-cell\">");
				formatter.increaseIndent();
				formatter.addLine(inputField);
				formatter.decreaseIndent();
				formatter.addLine("</div>");
				formatter.decreaseIndent();
				formatter.addLine("</div>");
			});

			formatter.decreaseIndent();
			formatter.addLine("</div>");
			formatter.addBlankLine();
		}

		formatter.addLine("<div class=\"form-field-table\">");
		formatter.increaseIndent();
		formatter.addLine("<div class=\"form-field-row\">");
		formatter.increaseIndent();

		final var refreshButton = new StringBuilder("<button pButton type=\"button\" icon=\"pi pi-refresh\" ");
		refreshButton.append("(click)=\"");

		if (addQuickSearch)
			refreshButton.append("performQuickSearch()");
		else if (addAdvSearch)
			refreshButton.append("performSearchOperation(searchInput)");
		else
			refreshButton.append("performSearchOperation()");

		refreshButton.append("\" ");
		refreshButton.append("i18n-label=\"@@button_refresh\" label=\"Refresh\"></button>");

		formatter.addLine("<div class=\"form-field-cell\">");
		formatter.increaseIndent();
		formatter.addLine(refreshButton.toString());
		formatter.decreaseIndent();
		formatter.addLine("</div>");

		if (addAdvSearch) {
			final var searchButton = new StringBuilder("<button pButton type=\"button\" icon=\"pi pi-search\" ");
			searchButton.append("(click)=\"showSearchInputDialog = !showSearchInputDialog\" ");
			searchButton.append("i18n-label=\"@@button_search\" label=\"Search\"></button>");

			formatter.addLine("<div class=\"form-field-cell\">");
			formatter.increaseIndent();
			formatter.addLine(searchButton.toString());
			formatter.decreaseIndent();
			formatter.addLine("</div>");
		}

		formatter.decreaseIndent();
		formatter.addLine("</div>");
		formatter.decreaseIndent();
		formatter.addLine("</div>");
		formatter.addBlankLine();
		formatter.addLine("<div class=\"form-spacer\"></div>");
		formatter.addBlankLine();
		formatter.addLine("<p-tree [value]=\"rootNodes\" selectionMode=\"single\" [(selection)]=\"selectedNode\"");
		formatter.increaseIndent();

		if (!dropItems.isEmpty())
			formatter.addLine("[draggableNodes]=\"true\" [droppableNodes]=\"true\" (onNodeDrop)=\"handleNodeDropEvent($event)\"");

		if (!rootItem.getChildren().isEmpty() || recursiveStructure)
			formatter.addLine("(onNodeExpand)=\"expandNode($event)\"");

		formatter.addLine("[style]=\"{'width':'100%', 'min-height': '420px'}\">");
		formatter.addBlankLine();
		formatter.addLine("<ng-template let-node pTemplate=\"default\">");
		formatter.increaseIndent();
		formatter.addLine("<i class=\"pi pi-file\"></i>&nbsp;");
		formatter.addLine("<span style=\"vertical-align: middle;\">{{node.label}}</span>");
		formatter.decreaseIndent();
		formatter.addLine("</ng-template>");

		allTreeItems.forEach(treeItem -> {
			final var nodeTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";

			var itemSpan = "<span style=\"vertical-align: middle;\" (contextmenu)=\"showContextMenu($event, node)\">";
			itemSpan += "{{node.label}}</span>";

			formatter.addLine("<ng-template let-node pTemplate=\"" + nodeTypeName + "\">");
			formatter.increaseIndent();
			formatter.addLine("<i class=\"pi pi-folder\"></i>&nbsp;");
			formatter.addLine(itemSpan);
			formatter.decreaseIndent();
			formatter.addLine("</ng-template>");

			if (hasAction(treeItem, true)) {
				final var folderTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

				formatter.addLine("<ng-template let-node pTemplate=\"" + folderTypeName + "\">");
				formatter.increaseIndent();
				formatter.addLine("<i class=\"pi pi-file\"></i>&nbsp;");
				formatter.addLine(itemSpan);
				formatter.decreaseIndent();
				formatter.addLine("</ng-template>");
			}
		});

		formatter.decreaseIndent();
		formatter.addBlankLine();
		formatter.addLine("</p-tree>");
		formatter.addBlankLine();

		// Add a status field
		formatter.addLine("<div class=\"form-field-table\">");
		formatter.increaseIndent();
		formatter.addLine("<div class=\"form-field-row\">");
		formatter.increaseIndent();
		formatter.addLine("<div class=\"form-field-cell label-field-mandatory\">");
		formatter.increaseIndent();
		formatter.addLine("{{statusText}}");
		formatter.decreaseIndent();
		formatter.addLine("</div>");
		formatter.decreaseIndent();
		formatter.addLine("</div>");
		formatter.decreaseIndent();
		formatter.addLine("</div>");
		formatter.addBlankLine();

		boolean hasContextMenu = false;

		// Create the context menus
		for (final TreeViewItem treeItem : treeItemsWithActions) {
			if (hasAction(treeItem, false)) {
				final var contextMenuName = "contextMenu" + treeItem.getItemDTO().getDomainObject().getName();
				final var menuItemArrayName = "menuItems" + treeItem.getItemDTO().getDomainObject().getName();
				hasContextMenu = true;

				formatter.addLine("<p-contextMenu #" + contextMenuName + " [model]=\"" + menuItemArrayName + "\"></p-contextMenu>");
			}

			if (hasAction(treeItem, true)) {
				var contextMenuName = "contextMenu" + treeItem.getItemDTO().getName();
				var menuItemArrayName = "menuItems" + treeItem.getItemDTO().getName();
				hasContextMenu = true;

				if (treeItem.getAssociation() != null) {
					contextMenuName = "contextMenu" + treeItem.getAssociation().getUpperCaseName();
					menuItemArrayName = "menuItems" + treeItem.getAssociation().getUpperCaseName();
				}

				formatter.addLine("<p-contextMenu #" + contextMenuName + " [model]=\"" + menuItemArrayName + "\"></p-contextMenu>");
			}
		}

		if (hasContextMenu)
			formatter.addBlankLine();

		formatter.decreaseIndent();
		formatter.addLine("</cc-view-container>");

		final WorkspaceFile templateFile = tree.getUserInterfaceFile();
		templateFile.setContent(formatter.getContent());

		EclipseIDEService.createOrUpdateFile(templateFile);
	}

}
