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
package net.codecadenza.eclipse.generator.client.imp.vaadin.view;

import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getAddForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getCreateNewForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDeleteMethod;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getEditForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getReadOnlyForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getSubItemRemoveMethod;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.VaadinConstants.MAIN_VIEW;
import static net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator.TRANSLATION_KEYS;
import static net.codecadenza.eclipse.shared.Constants.MANAGED_SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SAVED_QUERY_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.TEXT_PREFIX;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.security.VaadinSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;

/**
 * <p>
 * Generator for tree views of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinTreeViewGenerator extends AbstractTreeViewGenerator {
	private static final String NODE_TYPE_DATA = "DATA_TYPE_NODE";
	private static final String RECURSIVE_METHOD_NAME = "addSubItemsToTree";

	private final VaadinI18NGenerator i18n;
	private final VaadinSecurityHelper securityHelper;
	private final HashSet<String> injectedServices = new HashSet<>();
	private final Map<TreeViewItem, Set<Form>> formActionsOfItem = new HashMap<>();
	private final Map<TreeViewItem, Set<Form>> formActionsOfParent = new HashMap<>();
	private final Map<TreeViewItem, Set<BoundaryMethod>> methodActionsOfItem = new HashMap<>();
	private final Map<TreeViewItem, Set<BoundaryMethod>> methodActionsOfParent = new HashMap<>();
	private final Set<TreeViewItem> treeItemsWithActions = new HashSet<>();
	private final Set<BoundaryMethod> actionMethods = new HashSet<>();
	private final String locale;

	/**
	 * Constructor
	 * @param tree
	 */
	public VaadinTreeViewGenerator(TreeView tree) {
		super(tree);

		this.securityHelper = new VaadinSecurityHelper(project);
		this.addSecurity = securityHelper.isSecurityAdded();
		this.i18n = new VaadinI18NGenerator(project);
		this.locale = i18n.getLocaleFragment();

		if (addSecurity && (addAdvSearch || addQuickSearch)
				&& (project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY) != null))
			this.saveQueries = true;

		initActions(rootItem);

		initializeInternalFields();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS);
		importPackage(tree.getDTO().getNamespace().toString());
		importPackage(project.getClientNamespace().toString());
		importPackage("java.util");
		importPackage("net.codecadenza.runtime.webclient.vaadin.dialog");
		importPackage("net.codecadenza.runtime.webclient.vaadin.i18n");
		importPackage("net.codecadenza.runtime.webclient.vaadin.tree");
		importPackage("net.codecadenza.runtime.webclient.vaadin.util");
		importPackage("com.vaadin.flow.component.icon");
		importPackage("com.vaadin.flow.router");

		addImports(securityHelper.getSecurityImports());

		if (addQuickSearch || addAdvSearch) {
			importPackage("net.codecadenza.runtime.search.dto");
			importPackage("com.vaadin.flow.component.button");
		}

		if (addQuickSearch) {
			importPackage("net.codecadenza.runtime.webclient.vaadin.component");
			importPackage("com.vaadin.flow.component");
			importPackage("com.vaadin.flow.component.formlayout");
			importPackage("com.vaadin.flow.component.formlayout.FormLayout");
			importPackage("com.vaadin.flow.component.textfield");
		}

		if (addAdvSearch)
			importPackage("com.vaadin.flow.component.orderedlayout");

		if (tree.needsSearchObject())
			importPackage("net.codecadenza.runtime.search.dto");

		if (saveQueries)
			importPackage(project.getRootNamespace().toString() + PACK_SERVICE);

		if (!formActionsOfParent.isEmpty() || !formActionsOfItem.isEmpty())
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

		addSubItemDTOImports(rootItem.getChildren());
		addSubItemBoundaryImports(rootItem.getChildren());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Route(value = " + tree.getName() + ".ROUTE, layout = " + MAIN_VIEW + ".class)\n");
		b.append("public class " + tree.getName());
		b.append(" extends AbstractTreeView implements BeforeEnterObserver, HasDynamicTitle");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		var folderTypeName = rootItem.getItemDTO().getName().toUpperCase() + "_TYPE";

		addPublicConstant(JavaType.STRING, "ROUTE", "\"tree/" + tree.getName() + "\"").create();
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addProtectedConstant(JavaType.STRING, NODE_TYPE_DATA, "\"" + NODE_TYPE_DATA + "\"").create();
		addPrivateConstant(JavaType.STRING, folderTypeName, "\"" + folderTypeName + "\"").create();

		if (recursiveStructure) {
			folderTypeName = rootItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			addPrivateConstant(JavaType.STRING, folderTypeName, "\"" + folderTypeName + "\"").create();
		}

		addStaticFields(rootItem.getChildren());
		addProtectedConstant(JavaType.STRING, "ITEM_LABEL_SEPARATOR", "\": \"").create();

		if (addSecurity)
			addProtectedField(SECURITY_MANAGER, MANAGED_SECURITY_MANAGER).inject().create();

		if (saveQueries) {
			addPublicConstant(JavaType.STRING, "ID",
					"\"" + project.getClientNamespace().toString() + PACK_CLIENT_TREE + "." + tree.getName() + "\"").create();
			addPrivateField(SAVED_QUERY_SERVICE, "queryManager").inject().create();
		}

		addInjectedService(tree.getBoundaryMethod());
		addFields(rootItem);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final List<String> injectedFieldsOfSuperClass = List.of("I18NService i18n", "PreferencesStore preferences");

		addDefaultConstructorForInjection(injectedFieldsOfSuperClass);
		addConstructorForInjection(null, injectedFieldsOfSuperClass);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final String permissionCheck = securityHelper.addFormPermissionCheck(i18n, tree.getRoles());
		var methodSignature = "String getPageTitle()";

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.HasDynamicTitle#getPageTitle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return " + i18n.getI18N(tree) + ";\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void beforeEnter(BeforeEnterEvent event)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see com.vaadin.flow.router.internal.BeforeEnterHandler#beforeEnter(com.vaadin.flow.router.BeforeEnterEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		if (!permissionCheck.isEmpty()) {
			b.append(permissionCheck);
			b.append("\n");
		}

		if (saveQueries) {
			final String logOnPKGetter = project.getApplicationLogOnDTO().getPKAttribute().getModelGetterName();

			b.append("try\n");
			b.append("{\n");
			b.append("searchInput = queryManager.getLastQuery(" + MANAGED_SECURITY_MANAGER);
			b.append(".getLogOnDTO()." + logOnPKGetter + ", ID);\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while loading saved query!", "e");

			b.append("}\n\n");
		}

		b.append("addRootTreeItems();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (addQuickSearch)
			addQuickSearchMethod();

		if (addAdvSearch) {
			addAdvancedSearch();
			addCountMethod();
		}

		if (!rootItem.getChildren().isEmpty() || recursiveStructure) {
			boolean firstItem = true;
			methodSignature = "void onNodeExpand(TreeItem expandedItem)";
			b = new StringBuilder();

			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.webclient.vaadin.tree.AbstractTreeView#");
			b.append("onNodeExpand(net.codecadenza.runtime.webclient.vaadin.tree.TreeItem)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("final String type = expandedItem.getType();\n\n");

			if (recursiveStructure) {
				final var nodeTypeName = rootItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

				b.append("if(type.equals(" + nodeTypeName + "))\n");
				b.append(RECURSIVE_METHOD_NAME + "(expandedItem);\n");

				firstItem = false;
			}

			b.append(addNodeExpandFragment(rootItem, firstItem));
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		addMethods(rootItem);

		if (recursiveStructure)
			addSubItemTreeBuildMethod(rootItem);

		addContextMenuItems();

		// Add the method that implements getTreeViewLogger()
		addGetLoggerMethod("net.codecadenza.runtime.webclient.vaadin.tree.AbstractTreeView", "getTreeViewLogger");

		i18n.save();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator#addSubItemBoundaryImports(java.util.
	 * Collection)
	 */
	@Override
	protected void addSubItemBoundaryImports(Collection<TreeViewItem> items) {
		super.addSubItemBoundaryImports(items);

		items.forEach(treeItem -> addSubItemBoundaryImports(treeItem.getChildren()));
	}

	/**
	 * @param method
	 */
	private void addInjectedService(BoundaryMethod method) {
		final String injectedService = method.getBoundaryBean().getInterfaceName();

		if (!injectedServices.contains(injectedService))
			new ServiceDeclarationGenerator(this, method.getBoundaryBean()).addField();

		injectedServices.add(injectedService);
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
	public void addFormAction(TreeViewItem treeItem, Form form, boolean addToParent) {
		if (form == null)
			return;

		treeItemsWithActions.add(treeItem);

		if (addToParent) {
			formActionsOfParent.computeIfAbsent(treeItem, key -> new HashSet<>());
			formActionsOfParent.get(treeItem).add(form);
		}
		else {
			formActionsOfItem.computeIfAbsent(treeItem, key -> new HashSet<>());
			formActionsOfItem.get(treeItem).add(form);
		}
	}

	/**
	 * Add a method action for the given tree item to the internal data structure
	 * @param treeItem
	 * @param method
	 * @param addToParent
	 */
	public void addMethodAction(TreeViewItem treeItem, BoundaryMethod method, boolean addToParent) {
		if (method == null)
			return;

		treeItemsWithActions.add(treeItem);
		actionMethods.add(method);

		if (addToParent) {
			methodActionsOfParent.computeIfAbsent(treeItem, key -> new HashSet<>());
			methodActionsOfParent.get(treeItem).add(method);
		}
		else {
			methodActionsOfItem.computeIfAbsent(treeItem, key -> new HashSet<>());
			methodActionsOfItem.get(treeItem).add(method);
		}
	}

	/**
	 * @param items
	 */
	private void addStaticFields(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			final var nodeTypeName = item.getItemDTO().getName().toUpperCase() + "_TYPE";

			addPrivateConstant(JavaType.STRING, nodeTypeName, "\"" + nodeTypeName + "\"").create();

			final var nodeParentTypeName = item.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			addPrivateConstant(JavaType.STRING, nodeParentTypeName, "\"" + nodeParentTypeName + "\"").create();

			addStaticFields(item.getChildren());
		});
	}

	/**
	 * @param parentItem
	 */
	private void addFields(TreeViewItem parentItem) {
		parentItem.getChildren().forEach(treeItem -> {
			BoundaryMethod method = null;

			if (treeItem.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
				method = getDeleteMethod(treeItem);
			else
				method = getSubItemRemoveMethod(treeItem);

			if (method != null)
				addInjectedService(method);

			addInjectedService(treeItem.getDataFetchMethod());
			addFields(treeItem);
		});
	}

	/**
	 * @param parentTreeItem
	 * @param firstItem
	 * @return the generated content
	 */
	private String addNodeExpandFragment(TreeViewItem parentTreeItem, boolean firstItem) {
		final var b = new StringBuilder();

		for (final TreeViewItem treeItem : parentTreeItem.getChildren()) {
			final var nodeTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			if (firstItem) {
				firstItem = false;

				b.append("if");
			}
			else
				b.append("else if");

			b.append("(type.equals(" + nodeTypeName + "))\n");
			b.append("add" + treeItem.getAssociation().getUpperCaseName() + "ToTree(expandedItem);\n");
			b.append(addNodeExpandFragment(treeItem, false));
		}

		return b.toString();
	}

	/**
	 * @param attr
	 * @param skipLabel
	 * @param label
	 * @return the generated content
	 */
	private String createItemText(DTOBeanAttribute attr, boolean skipLabel, String label) {
		final var b = new StringBuilder();
		final var getter = "item." + attr.getGetterName();
		final boolean addNullCheck = !attr.getSearchType().isPrimitive();

		if (!skipLabel) {
			final var key = attr.getDTOBean().getName() + "_" + attr.getName();

			if (label == null || label.isEmpty())
				label = attr.getDomainAttribute().getLabel().substring(0, 1).toUpperCase()
						+ attr.getDomainAttribute().getLabel().substring(1);

			b.append("itemText.append(");
			b.append(i18n.getI18NMessage(key, label) + " + ITEM_LABEL_SEPARATOR);\n");

			if (addNullCheck)
				b.append("\n");
		}

		if (addNullCheck)
			b.append("if(" + getter + " != null)\n");

		b.append("itemText.append(" + attr.getDomainAttribute().convertToString(getter) + ");\n");

		if (addNullCheck)
			b.append("\n");

		return b.toString();
	}

	/**
	 * Add the method for searching tree view items
	 */
	private void addQuickSearchMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "Component getQuickSearchPanel()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.tree.AbstractTreeView#getQuickSearchPanel()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var flFields = new FormLayout();\n");
		b.append("flFields.setResponsiveSteps(new ResponsiveStep(\"1px\", 1));\n\n");

		tree.getQuickSearchItems().forEach(treeItem -> {
			final String inputFieldName = TEXT_PREFIX + treeItem.getDTOAttribute().getUpperCaseName();
			final var key = treeItem.getDTOAttribute().getDTOBean().getName() + "_qsi_" + treeItem.getDTOAttribute().getName();
			final String label = treeItem.getLabel();

			b.append("final var " + inputFieldName + " = new TextField();\n");
			b.append(inputFieldName + ".setWidthFull();\n");
			b.append(inputFieldName + ".setId(\"" + inputFieldName + "\");\n\n");
			b.append("flFields.addFormItem(" + inputFieldName + ", " + i18n.getI18NMessage(key, label, true) + ");\n\n");
		});

		b.append("final var cmdSearch = new Button(" + i18n.getI18NMessage("cmd_search", "Search") + ");\n");
		b.append("cmdSearch.setIcon(new Icon(VaadinIcon.SEARCH));\n");
		b.append("cmdSearch.setId(\"cmdSearch\");\n\n");
		b.append("cmdSearch.addClickListener(event ->\n");
		b.append("{\n");
		b.append("// Initialize search object\n");
		b.append("searchInput = new SearchDTO();\n");
		b.append("searchInput.setMaxResult(1000);\n");
		b.append("searchInput.setExactFilterMatch(true);\n");
		b.append("searchInput.setCaseSensitive(false);\n");
		b.append("searchInput.setCount(false);\n");
		b.append("searchInput.setDateFormat(preferences.getDateFormat());\n");
		b.append("searchInput.setDateTimeFormat(preferences.getDateTimeFormat());\n");
		b.append("searchInput.setNumberFormat(preferences.getNumberFormat());\n");

		int itemIndex = 1;

		for (final TreeSearchItem treeItem : tree.getQuickSearchItems()) {
			final String inputFieldName = TEXT_PREFIX + treeItem.getDTOAttribute().getUpperCaseName();
			final var searchFieldName = "field" + itemIndex;

			b.append("\n");
			b.append("final var " + searchFieldName + " = new SearchFieldDTO(" + (itemIndex - 1) + ", ");
			b.append(treeItem.getDTOAttribute().getSelectTokenConstant() + ", \"\", ");
			b.append("SearchFieldDataTypeEnum.STRING, 0);\n");
			b.append(searchFieldName + ".setFilterCriteria(" + inputFieldName + ".getValue());\n\n");
			b.append("searchInput.getSearchFields().add(" + searchFieldName + ");\n");

			itemIndex++;
		}

		b.append("\n");
		b.append("addRootTreeItems();\n");
		b.append("});\n\n");
		b.append("final var panQuickSearch = new BorderPanel(" + i18n.getI18NMessage("pan_quick_search", "Quick search") + ");\n");
		b.append("panQuickSearch.setWidthFull();\n");
		b.append("panQuickSearch.add(flFields, cmdSearch);\n\n");
		b.append("return panQuickSearch;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method for searching tree view items
	 */
	private void addAdvancedSearch() {
		final var b = new StringBuilder();
		final var methodSignature = "void addHeaderButtons(HorizontalLayout hlButtons)";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.tree.AbstractTreeView#");
		b.append("com.vaadin.flow.component.orderedlayout.HorizontalLayout)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super.addHeaderButtons(hlButtons);\n\n");
		b.append("final var cmdSearch = new Button(" + i18n.getI18NMessage("cmd_search", "Search") + ");\n");
		b.append("cmdSearch.setId(\"cmdOpenSearchDialog\");\n");
		b.append("cmdSearch.setIcon(new Icon(VaadinIcon.SEARCH));\n\n");
		b.append("cmdSearch.addClickListener(event ->\n");
		b.append("{\n");
		b.append("searchInput = new SearchDTO();\n");
		b.append("var fieldLabel = \"\";\n\n");
		b.append("// Initialize search object\n");
		b.append("searchInput.setMaxResult(1000);\n");
		b.append("searchInput.setExactFilterMatch(true);\n");
		b.append("searchInput.setCaseSensitive(false);\n");
		b.append("searchInput.setCount(true);\n");
		b.append("searchInput.setDateFormat(preferences.getDateFormat());\n");
		b.append("searchInput.setDateTimeFormat(preferences.getDateTimeFormat());\n");
		b.append("searchInput.setNumberFormat(preferences.getNumberFormat());\n");
		b.append("\n");

		int fieldIndex = 1;

		// Add all advanced search fields
		for (final TreeSearchItem treeItem : tree.getAdvancedSearchItems()) {
			final DomainAttribute attribute = treeItem.getDTOAttribute().getDomainAttribute();
			final var key = treeItem.getDTOAttribute().getDTOBean().getName() + "_asi_" + treeItem.getDTOAttribute().getName();
			final String label = treeItem.getLabel();

			b.append("fieldLabel = " + i18n.getI18NMessage(key, label) + ";\n\n");
			b.append("final var field" + fieldIndex + " = new SearchFieldDTO(" + fieldIndex + ", ");
			b.append(treeItem.getDTOAttribute().getSelectTokenConstant());
			b.append(", fieldLabel, ");
			b.append(attribute.getSearchFieldDataType());
			b.append(", 0);\n");

			if (attribute.getJavaType().isEnum()) {
				final var javaEnum = (JavaEnum) attribute.getJavaType();

				// Generate translations for all literals
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);

				b.append("\nfinal var enumListValues" + fieldIndex + " = new HashMap<String, String>();\n");

				for (final EnumLiteral value : javaEnum.getEnumerationValues()) {
					b.append("enumListValues" + fieldIndex + ".put(\"" + value.getName() + "\", i18n.getTranslation");
					b.append("(" + javaEnum.getName().toUpperCase() + "_" + value.getName().toUpperCase() + "));\n");
				}

				b.append("\n");
				b.append("field" + fieldIndex + ".setEnumListValues(enumListValues" + fieldIndex + ");\n\n");
			}

			if (attribute.getJavaType().isLocalDate() || attribute.getTemporalType() == TemporalTypeEnumeration.DATE)
				b.append("field" + fieldIndex + ".setDateTimeFormat(false);\n\n");

			b.append("searchInput.getSearchFields().add(field" + fieldIndex + ");\n\n");

			fieldIndex++;
		}

		b.append("openSearchDialog();\n");
		b.append("});\n\n");
		b.append("hlButtons.add(cmdSearch);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the count method
	 */
	private void addCountMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "long performCountOperation()";
		final BoundaryMethod method = tree.getCountMethod();

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.tree.AbstractTreeView#performCountOperation()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return ");

		new ServiceInvocationGenerator(method, b).addInvocation("searchInput");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method to build the tree view
	 * @param treeItem
	 */
	private void addTreeBuildMethod(TreeViewItem treeItem) {
		final var b = new StringBuilder();
		final var methodSignature = "void addRootTreeItems()";

		if (!treeItem.isRootItem()) {
			addSubItemTreeBuildMethod(treeItem);
			return;
		}

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.tree.AbstractTreeView#addRootTreeItems()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final Long start = System.currentTimeMillis();\n");
		b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_add_to_tree", "Add items to tree view") + ";\n");
		b.append("List<" + treeItem.getItemDTO().getName() + "> list = null;\n");
		b.append("long itemCount = 0;\n");
		b.append("StringBuilder itemText;\n\n");
		b.append("// Remove all items from tree!\n");
		b.append("removeAllItems();\n\n");

		if (tree.needsSearchObject())
			if (addAdvSearch || addQuickSearch) {
				b.append("if(searchInput == null)\n");
				b.append("return;\n\n");
			}
			else {
				b.append("// Initialize search object\n");
				b.append("searchInput = new SearchDTO();\n");
				b.append("searchInput.setMaxResult(1000);\n");
				b.append("searchInput.setExactFilterMatch(true);\n");
				b.append("searchInput.setCaseSensitive(false);\n");
				b.append("searchInput.setCount(false);\n");
				b.append("searchInput.setDateFormat(preferences.getDateFormat());\n");
				b.append("searchInput.setDateTimeFormat(preferences.getDateTimeFormat());\n");
				b.append("searchInput.setNumberFormat(preferences.getNumberFormat());\n\n");
			}

		final BoundaryMethod method = tree.getBoundaryMethod();
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);
		final var errorMessage = i18n.getI18NMessage("msg_err_search_tree", "Searching for tree items failed!");

		b.append("try\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + treeItem.getItemDTO().getDomainObject().getLabel() + " items");

		b.append("\n");
		b.append("list = ");

		if (tree.needsSearchObject())
			invocationGenerator.addInvocation("searchInput");
		else
			invocationGenerator.addInvocation();

		b.append("\n");
		b.append("itemCount = list.size();\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while fetching data!", "e");

		b.append("\n");
		b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
		b.append("return;\n");
		b.append("}\n\n");

		if (saveQueries) {
			final String logOnPKGetter = project.getApplicationLogOnDTO().getPKAttribute().getModelGetterName();

			b.append("// Save query\n");
			b.append("try\n");
			b.append("{\n");
			b.append("queryManager.saveQuery(" + MANAGED_SECURITY_MANAGER + ".getLogOnDTO().");
			b.append(logOnPKGetter + ", ID, null, searchInput);\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while saving query!", "e");

			b.append("}\n\n");
		}

		b.append("for(final " + treeItem.getItemDTO().getName() + " item : list)\n");
		b.append("{\n");
		b.append(addDisplayAttributes(treeItem));

		final var itemTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";
		final DTOBeanAttribute pkAttr = treeItem.getItemDTO().getPKAttribute();

		b.append("final var treeItem = new TreeItem(");
		b.append(pkAttr.getDomainAttribute().convertToString("item." + pkAttr.getGetterName()));
		b.append(", " + itemTypeName + ", itemText.toString());\n");
		b.append("treeItem.setIcon(VaadinIcon.FOLDER);\n\n");
		b.append("addItem(null, treeItem);\n");

		treeItem.getNodes().forEach(node -> {
			final var itemName = "treeItem" + node.getDTOAttribute().getUpperCaseName();

			b.append("\n// Add tree view item \"" + node.getLabel() + "\"\n");
			b.append("itemText = new StringBuilder();\n");
			b.append(createItemText(node.getDTOAttribute(), false, node.getLabel()) + "\n");
			b.append("final var " + itemName + " = new TreeItem(");
			b.append(pkAttr.getDomainAttribute().convertToString("item." + pkAttr.getGetterName()));
			b.append(", " + NODE_TYPE_DATA + ", itemText.toString());\n");
			b.append(itemName + ".setIcon(VaadinIcon.FILE);\n\n");
			b.append("addItem(treeItem, " + itemName + ");\n");
		});

		b.append(addSubItemFolderNodes(treeItem));
		b.append("}\n\n");
		b.append("refreshTree();\n");
		b.append("lblFooter.setText(buildFooterMessage(itemCount, null, System.currentTimeMillis() - start));\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * @param treeItem
	 * @return the generated content
	 */
	private String addDisplayAttributes(TreeViewItem treeItem) {
		final var b = new StringBuilder();
		final int numberOfDisplayAttributes = treeItem.getDisplayAttributes().size();
		boolean isFirstAttribute = true;
		int attributeIndex = 0;

		b.append("itemText = new StringBuilder();\n\n");

		for (final DTOBeanAttribute attr : treeItem.getDisplayAttributes()) {
			b.append(createItemText(attr, isFirstAttribute, null));

			if (numberOfDisplayAttributes > 1) {
				// The text for all subsequent attributes should be surrounded by parentheses!
				if (isFirstAttribute)
					b.append("itemText.append(\" (\");\n");

				// Add a whitespace character before adding the text of subsequent attributes
				if (!isFirstAttribute && (attributeIndex + 1) < numberOfDisplayAttributes)
					b.append("itemText.append(\" \");\n");
			}

			attributeIndex++;
			isFirstAttribute = false;
		}

		if (numberOfDisplayAttributes > 1)
			b.append("itemText.append(\")\");\n");

		b.append("\n");

		return b.toString();
	}

	/**
	 * @param treeItem
	 */
	private void addSubItemTreeBuildMethod(TreeViewItem treeItem) {
		final var b = new StringBuilder();
		final var errorMessage = i18n.getI18NMessage("msg_err_search_tree", "Searching for tree items failed!");
		final var noObjectsFoundMessage = i18n.getI18NMessage("msg_no_result", "No objects found!");
		String methodSignature;
		BoundaryMethod method = treeItem.getDataFetchMethod();
		DomainAttribute parentPkAttr;

		if (method == null) {
			method = tree.getRecursiveMethod();
			methodSignature = "void " + RECURSIVE_METHOD_NAME + "(TreeItem parentItem)";
			parentPkAttr = treeItem.getItemDTO().getPKAttribute().getDomainAttribute();
		}
		else {
			methodSignature = "void add" + treeItem.getAssociation().getUpperCaseName() + "ToTree(TreeItem parentItem)";
			parentPkAttr = treeItem.getParentItem().getItemDTO().getPKAttribute().getDomainAttribute();
		}

		b.append("/**\n");
		b.append(" * Add " + treeItem.getItemDTO().getDomainObject().getLabel() + " objects to parent tree view item\n");
		b.append(" * @param parentItem\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + treeItem.getItemDTO().getDomainObject().getLabel() + " sub-items");

		b.append("\n");
		b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_add_to_tree", "Add items to tree view") + ";\n");
		b.append("List<" + treeItem.getItemDTO().getName() + "> list = null;\n");
		b.append("StringBuilder itemText;\n\n");
		b.append("// Remove all items of selected parent\n");
		b.append("removeItemsOfParent(parentItem);\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("list = ");

		new ServiceInvocationGenerator(method, b).addInvocation(parentPkAttr.convertFromString("parentItem.getId()"));

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while fetching data!", "e");

		b.append("\n");
		b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("if(list.isEmpty())\n");
		b.append("{\n");

		addDebugLog(b, "Search operation returned with no records!");

		b.append("\n");
		b.append("new InfoMessageDialog(dialogTitle, " + noObjectsFoundMessage + ", " + locale + ").open();\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("for(final " + treeItem.getItemDTO().getName() + " item : list)\n");
		b.append("{\n");
		b.append(addDisplayAttributes(treeItem));

		final var itemTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";
		final DTOBeanAttribute pkAttr = treeItem.getItemDTO().getPKAttribute();

		b.append("final var treeItem = new TreeItem(");
		b.append(pkAttr.getDomainAttribute().convertToString("item." + pkAttr.getGetterName()));
		b.append(", " + itemTypeName + ", itemText.toString());\n");
		b.append("treeItem.setIcon(VaadinIcon.FOLDER);\n\n");
		b.append("addItem(parentItem, treeItem);\n");

		treeItem.getNodes().forEach(node -> {
			final var itemName = "treeItem" + node.getDTOAttribute().getUpperCaseName();

			b.append("\n// Add tree view item \"" + node.getLabel() + "\"\n");
			b.append("itemText = new StringBuilder();\n");
			b.append(createItemText(node.getDTOAttribute(), false, node.getLabel()) + "\n");
			b.append("final var " + itemName + " = new TreeItem(");
			b.append(pkAttr.getDomainAttribute().convertToString("item." + pkAttr.getGetterName()));
			b.append(", " + NODE_TYPE_DATA + ", itemText.toString());\n");
			b.append(itemName + ".setIcon(VaadinIcon.FILE);\n\n");
			b.append("addItem(treeItem, " + itemName + ");\n");
		});

		b.append(addSubItemFolderNodes(treeItem));
		b.append("}\n\n");
		b.append("refreshTree();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * @param parentItem
	 * @return the generated content
	 */
	private String addSubItemFolderNodes(TreeViewItem parentItem) {
		final var b = new StringBuilder();
		final DTOBeanAttribute pkAttr = parentItem.getItemDTO().getPKAttribute();
		int subNodeIndex = 1;

		for (final TreeViewItem item : parentItem.getChildren()) {
			final var itemName = "subNode" + subNodeIndex;
			final var key = tree.getName() + "_" + item.getAssociation().getName();
			final String label = item.getLabel();
			final var nodeTypeName = item.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			b.append("\n// Add folder node for items of association \"" + item.getAssociation().getName() + "\"\n");
			b.append("final var " + itemName + " = new TreeItem(");
			b.append(pkAttr.getDomainAttribute().convertToString("item." + pkAttr.getGetterName()));
			b.append(", " + nodeTypeName + "," + i18n.getI18NMessage(key, label) + ");\n");
			b.append(itemName + ".setIcon(VaadinIcon.FOLDER);\n\n");
			b.append("addItem(treeItem, " + itemName + ", true);\n");

			subNodeIndex++;
		}

		// Add a folder for items of a recursive structure
		if (recursiveStructure && parentItem.isRootItem()) {
			final var itemName = "subNode" + subNodeIndex;
			final var key = tree.getName() + "_" + tree.getDomainObject().getName();
			final String label = tree.getDomainObject().getLabelPlural().substring(0, 1).toUpperCase()
					+ tree.getDomainObject().getLabelPlural().substring(1);
			final var nodeTypeName = rootItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			b.append("\n// Add folder node for items of recursive structure\n");
			b.append("final var " + itemName + " = new TreeItem(");
			b.append(pkAttr.getDomainAttribute().convertToString("item." + pkAttr.getGetterName()));
			b.append(", " + nodeTypeName + ", " + i18n.getI18NMessage(key, label) + ");\n");
			b.append(itemName + ".setIcon(VaadinIcon.FOLDER);\n\n");
			b.append("addItem(treeItem, " + itemName + ", true);\n");
		}

		return b.toString();
	}

	/**
	 * @param treeItem
	 * @param form
	 * @param method
	 * @return the corresponding method name for a given action
	 */
	private String createActionMethodName(TreeViewItem treeItem, Form form, BoundaryMethod method) {
		if (form != null) {
			if (form.getFormType() == FormTypeEnumeration.CREATE)
				return "create" + treeItem.getItemDTO().getDomainObject().getName();
			else if (form.getFormType() == FormTypeEnumeration.ADD)
				return "add" + treeItem.getItemDTO().getDomainObject().getName();
			else if (form.getFormType() == FormTypeEnumeration.READONLY)
				return "view" + treeItem.getItemDTO().getDomainObject().getName();
			else if (form.getFormType() == FormTypeEnumeration.UPDATE)
				return "edit" + treeItem.getItemDTO().getDomainObject().getName();
		}
		else {
			if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_PARENT) {
				if (treeItem.getAssociation() != null)
					return "add" + treeItem.getAssociation().getUpperCaseName() + "ToTree";

				return RECURSIVE_METHOD_NAME;
			}

			return method.getName();
		}

		return "";
	}

	/**
	 * Create the method for adding context menu items to the tree view
	 */
	private void addContextMenuItems() {
		final var b = new StringBuilder();
		final var methodSignature = "void addContextMenuItems(TreeItem selectedItem)";

		if (treeItemsWithActions.isEmpty())
			return;

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.webclient.vaadin.tree.AbstractTreeView#");
		b.append("addContextMenuItems(net.codecadenza.runtime.webclient.vaadin.tree.TreeItem)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		boolean firstEntry = true;

		// Create context menu items for all tree view items
		for (final TreeViewItem treeItem : treeItemsWithActions) {
			if ((formActionsOfItem.get(treeItem) == null || formActionsOfItem.get(treeItem).isEmpty())
					&& (methodActionsOfItem.get(treeItem) == null || methodActionsOfItem.get(treeItem).isEmpty()))
				continue;

			final var nodeType = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";

			if (firstEntry)
				firstEntry = false;
			else
				b.append("else ");

			b.append("if(selectedItem.getType().equals(" + nodeType + "))\n");
			b.append("{\n");

			if (formActionsOfItem.get(treeItem) != null)
				formActionsOfItem.get(treeItem).forEach(form -> b.append(addContextMenuItem(treeItem, form)));

			if (methodActionsOfItem.get(treeItem) != null)
				methodActionsOfItem.get(treeItem).forEach(method -> b.append(addContextMenuItem(treeItem, method)));

			b.append("}\n");
		}

		// Create context menu items for all parent tree view items
		for (final TreeViewItem treeItem : treeItemsWithActions) {
			if ((formActionsOfParent.get(treeItem) == null || formActionsOfParent.get(treeItem).isEmpty())
					&& (methodActionsOfParent.get(treeItem) == null || methodActionsOfParent.get(treeItem).isEmpty()))
				continue;

			final var nodeType = treeItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			if (firstEntry)
				firstEntry = false;
			else
				b.append("else ");

			b.append("if(selectedItem.getType().equals(" + nodeType + "))\n");
			b.append("{\n");

			if (formActionsOfParent.get(treeItem) != null)
				formActionsOfParent.get(treeItem).forEach(form -> b.append(addContextMenuItem(treeItem, form)));

			if (methodActionsOfParent.get(treeItem) != null)
				methodActionsOfParent.get(treeItem).forEach(method -> b.append(addContextMenuItem(treeItem, method)));

			b.append("}\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * @param treeItem
	 * @param form
	 * @return the generated content
	 */
	public String addContextMenuItem(TreeViewItem treeItem, Form form) {
		final var b = new StringBuilder();
		final String methodName = createActionMethodName(treeItem, form, null);

		if (form.getFormType() == FormTypeEnumeration.CREATE) {
			b.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_create", "Create") + ", ");
			b.append("event -> " + methodName + "());\n");
		}
		else if (form.getFormType() == FormTypeEnumeration.ADD) {
			b.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_add", "Add") + ", ");
			b.append("event -> " + methodName + "());\n");
		}
		else if (form.getFormType() == FormTypeEnumeration.READONLY) {
			b.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_view", "View") + ", ");
			b.append("event -> " + methodName + "());\n");
		}
		else if (form.getFormType() == FormTypeEnumeration.UPDATE) {
			b.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_edit", "Edit") + ", ");
			b.append("event -> " + methodName + "());\n");
		}

		final String content = securityHelper.wrapSecurityCode(form.getRoles(), b.toString());

		if (content.contains("if"))
			return content + "\n";

		return content;
	}

	/**
	 * @param treeItem
	 * @param method
	 * @return the generated content
	 */
	public String addContextMenuItem(TreeViewItem treeItem, BoundaryMethod method) {
		final var b = new StringBuilder();
		final String methodName = createActionMethodName(treeItem, null, method);

		if (method.getMethodType() == BoundaryMethodTypeEnumeration.DELETE) {
			b.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_delete", "Delete") + ", ");
			b.append("event -> " + methodName + "());\n");
		}
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.REMOVE_FROM_ASSOCIATION) {
			b.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_remove", "Remove") + ", ");
			b.append("event -> " + methodName + "());\n");
		}
		else if (method.getMethodType() == BoundaryMethodTypeEnumeration.FIND_BY_PARENT) {
			b.append("contextMenu.addItem(" + i18n.getI18NMessage("action_name_refresh", "Refresh") + ", ");
			b.append("event -> " + methodName + "(selectedItem));\n");
		}

		if (addSecurity && method.getMethodType() != BoundaryMethodTypeEnumeration.FIND_BY_PARENT
				&& method.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL) {
			final String content = securityHelper.wrapSecurityCode(method.getRoles(), b.toString());

			if (content.contains("if"))
				return content + "\n";

			return content;
		}

		return b.toString();
	}

	/**
	 * @param form
	 * @return the generated content
	 */
	private String createMethodBody(Form form) {
		final var b = new StringBuilder();
		final var noObjectSelectedMessage = i18n.getI18NMessage("msg_no_object_selected", "No object selected!");
		final var errorMessage = i18n.getI18NMessage("msg_err_open_dialog", "The selected dialog could not be opened!");

		b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_open_dialog", "Open dialog") + ";\n\n");

		if (form.getFormType() != FormTypeEnumeration.CREATE) {
			b.append("if(getSelectedItem() == null)\n");
			b.append("{\n");
			b.append("new InfoMessageDialog(dialogTitle, " + noObjectSelectedMessage + ", " + locale + ").open();\n");
			b.append("return;\n");
			b.append("}\n\n");
		}

		addDebugLog(b, "Open '" + form.getName() + "'");

		b.append("\n");
		b.append("try\n");
		b.append("{\n");

		if (form.getFormType() != FormTypeEnumeration.CREATE)
			b.append("navigateTo(" + form.getName() + ".class, getSelectedItem().getId());\n");
		else
			b.append("navigateTo(" + form.getName() + ".class);\n");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while opening '" + form.getName() + "'!", "e");

		b.append("\n");
		b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @param treeItem
	 */
	private void addMethods(TreeViewItem treeItem) {
		final var noObjectSelectedMessage = i18n.getI18NMessage("msg_no_object_selected", "No object selected!");
		final var objectDeletedMessage = i18n.getI18NMessage("msg_delete_obj", "Object deleted successfully!");
		final var errorMessage = i18n.getI18NMessage("msg_err_delete", "Could not delete selected object!");
		StringBuilder b;

		addTreeBuildMethod(treeItem);

		if (treeItem.getAssociation() == null
				|| treeItem.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional()) {
			final BoundaryMethod method = getDeleteMethod(treeItem);

			if (method != null) {
				final var methodSignature = "void " + method.getName() + "()";
				final DomainAttribute pkAttr = treeItem.getItemDTO().getPKAttribute().getDomainAttribute();

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Delete selected element\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ";\n\n");
				b.append("if(getSelectedItem() == null)\n");
				b.append("{\n");
				b.append("new InfoMessageDialog(dialogTitle, " + noObjectSelectedMessage + ", " + locale + ").open();\n");
				b.append("return;\n");
				b.append("}\n\n");
				b.append("final String dialogMsg = ");
				b.append(i18n.getI18NMessage("msg_confirm_delete", "Do you really want to delete the selected object?") + ";\n\n");
				b.append("final var dlg = new ConfirmationMessageDialog(dialogTitle, dialogMsg, " + locale + ");\n");
				b.append("dlg.open();\n\n");
				b.append("dlg.setButtonClickListener(type ->\n");
				b.append("{\n");
				b.append("if(type != ButtonType.YES)\n");
				b.append("return;\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("final String id = getSelectedItem().getId();\n\n");

				addDebugLog(b, "Delete selected object with id '{}'", "id");

				b.append("\n");

				new ServiceInvocationGenerator(method, b).addInvocation(pkAttr.convertFromString("id"));

				b.append("\n");
				b.append("// Remove selected item from tree view\n");
				b.append("removeSelectedItem();\n\n");
				b.append("new InfoMessageDialog(dialogTitle, " + objectDeletedMessage + ", " + locale + ").open();\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while deleting selected object!", "e");

				b.append("\n");
				b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
				b.append("}\n");
				b.append("});\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}
		else {
			final BoundaryMethod method = getSubItemRemoveMethod(treeItem);

			if (method != null) {
				final var methodSignature = "void " + method.getName() + "()";
				final DomainAttribute pkAttr = treeItem.getItemDTO().getPKAttribute().getDomainAttribute();
				final DomainAttribute pkParentAttr = treeItem.getParentItem().getItemDTO().getPKAttribute().getDomainAttribute();
				final var params = new ArrayList<String>();
				final String assocName = treeItem.getAssociation().getName();

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Remove " + treeItem.getItemDTO().getDomainObject().getLabel());
				b.append(" object from association \"" + assocName + "\"\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("final String dialogTitle = " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ";\n\n");
				b.append("if(getSelectedItem() == null)\n");
				b.append("{\n");
				b.append("new InfoMessageDialog(dialogTitle, " + noObjectSelectedMessage + ", " + locale + ").open();\n");
				b.append("return;\n");
				b.append("}\n\n");
				b.append("final String dialogMsg = ");
				b.append(i18n.getI18NMessage("msg_confirm_delete", "Do you really want to delete the selected object?") + ";\n\n");
				b.append("final var dlg = new ConfirmationMessageDialog(dialogTitle, dialogMsg, " + locale + ");\n");
				b.append("dlg.open();\n\n");
				b.append("dlg.setButtonClickListener(type ->\n");
				b.append("{\n");
				b.append("if(type != ButtonType.YES)\n");
				b.append("return;\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("final String id = getSelectedItem().getId();\n");
				b.append("final String parentId = getParentItem(getSelectedItem()).getId();\n\n");

				addDebugLog(b, "Remove selected object with id '{}' from list '" + assocName + "' of parent object with id '{}'", "id",
						"parentId");

				b.append("\n");

				params.add(pkParentAttr.convertFromString("parentId"));
				params.add(pkAttr.convertFromString("id"));

				new ServiceInvocationGenerator(method, b).addInvocation(params.stream().toArray(String[]::new));

				b.append("\n");
				b.append("// Remove selected item from tree view\n");
				b.append("removeSelectedItem();\n\n");
				b.append("new InfoMessageDialog(dialogTitle, " + objectDeletedMessage + ", " + locale + ").open();\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while removing selected object!", "e");

				b.append("\n");
				b.append("new ErrorMessageDialog(dialogTitle, " + errorMessage + ", e, " + locale + ").open();\n");
				b.append("}\n");
				b.append("});\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (getAddForm(treeItem) != null) {
			final var methodSignature = "void add" + treeItem.getItemDTO().getDomainObject().getName() + "()";
			final Form form = getAddForm(treeItem);

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Add " + treeItem.getItemDTO().getDomainObject().getLabel() + "\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append(createMethodBody(form));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (getEditForm(treeItem) != null) {
			final var methodSignature = "void edit" + treeItem.getItemDTO().getDomainObject().getName() + "()";
			final Form form = getEditForm(treeItem);

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Edit " + treeItem.getItemDTO().getDomainObject().getLabel() + "\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append(createMethodBody(form));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (getCreateNewForm(treeItem) != null && treeItem.isRootItem()) {
			final var methodSignature = "void create" + treeItem.getItemDTO().getDomainObject().getName() + "()";
			final Form form = getCreateNewForm(treeItem);

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Create new " + treeItem.getItemDTO().getDomainObject().getLabel() + "\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append(createMethodBody(form));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (getReadOnlyForm(treeItem) != null) {
			final var methodSignature = "void view" + treeItem.getItemDTO().getDomainObject().getName() + "()";
			final Form form = getReadOnlyForm(treeItem);

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * View " + treeItem.getItemDTO().getDomainObject().getLabel() + "\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append(createMethodBody(form));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		treeItem.getChildren().forEach(this::addMethods);
	}

}
