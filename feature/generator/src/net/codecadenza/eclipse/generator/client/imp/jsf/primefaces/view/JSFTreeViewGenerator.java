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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.view;

import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getAddForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getCreateNewForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDeleteMethod;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDownloadMethods;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getEditForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getReadOnlyForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getSubItemRemoveMethod;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.FORM_TITLE;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.TRANSLATION_KEYS_CLASS;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN_TYPE;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil.getAutoCompleteMethod;
import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.SAVED_QUERY_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.UI_TREE_FOLDER;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security.JSFSecurityGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TreeNode;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for tree views of a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFTreeViewGenerator extends AbstractTreeViewGenerator {
	private static final String SAVED_QUERY_MANAGER = "queryManager";
	private static final String VIEW_ID = "VIEW_ID";
	private static final String NODE_TYPE_DUMMY = "DUMMY_TYPE";
	private static final String NODE_TYPE_DATA = "DATA_TYPE_NODE";
	private static final String ROOT_NODE_NAME = "rootNode";
	private static final String SELECTED_NODE_NAME = "selectedNode";

	private final JSFI18NGenerator i18n;
	private final String managedBeanName;
	private final JSFSecurityGenerator securityHelper;
	private final HashSet<String> injectedServices = new HashSet<>();

	/**
	 * Constructor
	 * @param tree
	 */
	public JSFTreeViewGenerator(TreeView tree) {
		super(tree);

		this.managedBeanName = JSFGeneratorUtil.createManagedBeanName(tree.getName());
		this.securityHelper = new JSFSecurityGenerator(project);
		this.addSecurity = securityHelper.isSecurityAdded();
		this.i18n = new JSFI18NGenerator(project);

		if (addAdvSearch && addSecurity && project.getDomainObjectByTag(DomainTagEnumeration.SAVEDQUERY) != null)
			this.saveQueries = true;

		initializeInternalFields();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS_CLASS);
		importStatic(project.getClientNamespace().toString() + "." + USER_SESSION_BEAN_TYPE);
		importPackage(project.getClientNamespace().toString());
		importPackage("java.util");
		importPackage("java.io");
		importPackage("java.text");
		importPackage("org.primefaces.model");
		importClass("jakarta.faces.application.FacesMessage");
		importPackage("jakarta.inject");
		importPackage("net.codecadenza.runtime.webclient.primefaces.util");
		importPackage("net.codecadenza.runtime.webclient.primefaces.tree");
		importPackage(PACK_JAVA_TIME_FORMAT);

		if (project.isJakartaEEApplication())
			importPackage("jakarta.enterprise.context");
		else
			importClass("org.springframework.web.context.annotation.SessionScope");

		if (tree.needsSearchObject())
			importPackage("net.codecadenza.runtime.search.dto");

		if (addAdvSearch) {
			importPackage("net.codecadenza.runtime.webclient.primefaces.search");

			// Add further imports for auto-complete and enumeration search input fields
			for (final TreeSearchItem treeItem : tree.getAdvancedSearchItems()) {
				if (treeItem.getDTOAttribute().getDomainAttribute().getJavaType().isEnum()) {
					final var javaEnum = (JavaEnum) treeItem.getDTOAttribute().getDomainAttribute().getJavaType();
					importPackage(javaEnum.getNamespace().toString());
				}

				final BoundaryMethod m = getAutoCompleteMethod(treeItem.getDTOAttribute().getDomainAttribute());

				if (m == null)
					continue;

				final var listDTO = (DTOBean) m.getReturnType();

				if (project.isBoundaryMode())
					importPackage(listDTO.getNamespace().toString());
				else
					importPackage(listDTO.getDomainObject().getNamespace().toString());
			}

			if (saveQueries)
				importPackage(project.getRootNamespace().toString() + PACK_SERVICE);
		}

		importPackage(tree.getDTO().getNamespace().toString());

		addSubItemDTOImports(rootItem.getChildren());
		addSubItemBoundaryImports(rootItem.getChildren());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(tree.getName()) + "\")\n");

		if (project.isJakartaEEApplication())
			b.append("@SessionScoped\n");
		else
			b.append("@SessionScope\n");

		b.append("public class ");
		b.append(tree.getName());

		if (addAdvSearch)
			b.append(" extends AbstractSearchableView");

		b.append(" implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		var nodeTypeName = rootItem.getItemDTO().getName().toUpperCase() + "_TYPE";

		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		if (saveQueries) {
			final var viewId = project.getClientNamespace().toString() + PACK_CLIENT_TREE + "." + tree.getName();

			addPrivateConstant(JavaType.STRING, VIEW_ID, "\"" + viewId + "\"").create();
		}

		addProtectedConstant(JavaType.STRING, NODE_TYPE_DUMMY, "\"" + NODE_TYPE_DUMMY + "\"").create();
		addProtectedConstant(JavaType.STRING, NODE_TYPE_DATA, "\"" + NODE_TYPE_DATA + "\"").create();
		addPrivateConstant(JavaType.STRING, nodeTypeName, "\"" + nodeTypeName + "\"").create();

		if (recursiveStructure) {
			nodeTypeName = tree.getDomainObject().getName().toUpperCase() + "_FOLDER_TYPE";

			addPrivateConstant(JavaType.STRING, nodeTypeName, "\"" + nodeTypeName + "\"").create();
		}

		addStaticFields(rootItem.getChildren());

		final var pageURL = "\"" + UI_TREE_FOLDER + "/" + tree.getName() + ".jsf?faces-redirect=true\"";

		addProtectedConstant(JavaType.STRING, "ITEM_LABEL_SEPARATOR", "\": \"").create();
		addPublicConstant(JavaType.STRING, "PAGE_URL", pageURL).create();
		addPrivateField(JavaType.STRING, FORM_TITLE).create();
		addPrivateField(USER_SESSION_BEAN_TYPE, USER_SESSION_BEAN).inject().create();
		addPrivateField("TreeNode<" + JavaType.STRING + ">", ROOT_NODE_NAME).withTransientModifier().create();
		addPrivateField("TreeNode<TreeNavigatorItem>", SELECTED_NODE_NAME).withTransientModifier().create();
		addPrivateField(JavaType.INT, "itemCount").create();
		addPrivateField("ResourceBundle", "bundle").withTransientModifier().create();
		addProtectedField("DecimalFormat", "decimalFormat").withDefaultValue("new DecimalFormat()").create();
		addProtectedField("DateTimeFormatter", "dateTimeFormat").withTransientModifier().create();
		addProtectedField("DateTimeFormatter", "dateFormat").withTransientModifier().create();

		if (saveQueries)
			addPrivateField(SAVED_QUERY_SERVICE, SAVED_QUERY_MANAGER).withTransientModifier().inject().create();

		tree.getQuickSearchItems().forEach(searchItem -> {
			final var inputFieldName = searchItem.getDTOAttribute().getName() + "Filter";

			addPrivateField(JavaType.STRING, inputFieldName).create();
		});

		if (addAdvSearch) {
			addPrivateField(JavaType.LONG, "countResult").create();

			final var methodSet = new HashSet<BoundaryMethod>();

			// Add all necessary declarations for auto-complete search input fields
			for (final TreeSearchItem searchItem : tree.getAdvancedSearchItems()) {
				final BoundaryMethod m = getAutoCompleteMethod(searchItem.getDTOAttribute().getDomainAttribute());

				if (m == null || methodSet.contains(m))
					continue;

				methodSet.add(m);

				final BoundaryBean listBoundary = m.getBoundaryBean();

				if (!injectedServices.contains(listBoundary.getInterfaceName())) {
					new ServiceDeclarationGenerator(this, listBoundary).addField();

					injectedServices.add(listBoundary.getInterfaceName());
				}
			}
		}

		addInjectedService(tree.getBoundaryMethod());
		addFields(rootItem);

		dropItems.forEach(dropItem -> addInjectedService(dropItem.getDropMethod()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var methodSignature = "String getCurrentPageURL()";
		addGetterAndSetter(JavaType.STRING, FORM_TITLE, "the form title");
		addGetterAndSetter("TreeNode<" + JavaType.STRING + ">", ROOT_NODE_NAME, "the tree view's root node");
		addGetterAndSetter("TreeNode<TreeNavigatorItem>", SELECTED_NODE_NAME, "the selected tree node");
		addGetter(JavaType.INT, "itemCount", "the number of root items in tree view");

		tree.getQuickSearchItems().forEach(item -> {
			final var inputFieldName = item.getDTOAttribute().getName() + "Filter";
			final var comment = "the value of quick-search filter attribute \"" + item.getLabel() + "\"";

			addGetterAndSetter(JavaType.STRING, inputFieldName, comment);
		});

		if (addAdvSearch)
			addGetter(JavaType.LONG, "countResult", "the result of the count operation");

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the URL of the current page\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return PAGE_URL;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (addQuickSearch)
			addQuickSearchMethod();

		if (addAdvSearch)
			addAdvancedSearchMethod();

		methodSignature = "void refreshFormatSettings(SearchDTO searchInput)";

		if (tree.needsSearchObject()) {
			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Refresh format settings from user session\n");
			b.append(" * @param searchInput\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("searchInput.setDateFormat(" + USER_SESSION_BEAN + ".getDateFormat());\n");
			b.append("searchInput.setDateTimeFormat(" + USER_SESSION_BEAN + ".getDateTimeFormat());\n");
			b.append("searchInput.setNumberFormat(" + USER_SESSION_BEAN + ".getNumberFormat());\n");
			b.append("searchInput.setDecimalSeparator(java.text.DecimalFormatSymbols.getInstance(" + USER_SESSION_BEAN);
			b.append(".getLocale()).getDecimalSeparator());\n");
			b.append("searchInput.setGroupingSeparator(java.text.DecimalFormatSymbols.getInstance(" + USER_SESSION_BEAN);
			b.append(".getLocale()).getGroupingSeparator());\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		methodSignature = "void initView()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Initialize tree view");

		b.append("\n");
		b.append("bundle = ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, " + USER_SESSION_BEAN + ".getLocale());\n");
		b.append(securityHelper.addSecurityCheck(tree.getRoles()));
		b.append("\n");
		b.append(FORM_TITLE + " = ");

		if (addAdvSearch || addQuickSearch)
			b.append(i18n.getBundleFragment("form_title_seach", "Search for "));
		else
			b.append(i18n.getBundleFragment("form_title_list", "List of "));

		b.append(" + " + i18n.getI18NBundleFragment(tree.getDTO()) + " + ");
		b.append(i18n.getBundleFragment("form_title_objects", " objects") + ";\n\n");
		b.append("// Initialize formatters for items that represent number or date values\n");
		b.append("dateFormat = DateTimeFormatter.ofPattern(" + USER_SESSION_BEAN + ".getDateFormat())");
		b.append(".withZone(TimeZone.getTimeZone(" + USER_SESSION_BEAN + ".getTimeZone()).toZoneId());\n");
		b.append("dateTimeFormat = DateTimeFormatter.ofPattern(" + USER_SESSION_BEAN + ".getDateTimeFormat())");
		b.append(".withZone(TimeZone.getTimeZone(" + USER_SESSION_BEAN + ".getTimeZone()).toZoneId());\n");
		b.append("decimalFormat.applyPattern(" + USER_SESSION_BEAN + ".getNumberFormat());\n");

		if (!addAdvSearch && !addQuickSearch) {
			b.append("\n");
			b.append("add" + rootItem.getItemDTO().getDomainObject().getNamePlural() + "ToTree();\n");
		}

		if (saveQueries) {
			b.append("\n");
			b.append("// Check if previous search exists!\n");
			b.append("final SearchDTO lastSearch = " + SAVED_QUERY_MANAGER + ".getLastQuery(" + USER_SESSION_BEAN);
			b.append(".getPrincipal()." + project.getApplicationLogOnDTO().getPKAttribute().getGetterName());
			b.append(", " + VIEW_ID + ");\n\n");
			b.append("if(lastSearch != null)\n");
			b.append("{\n");
			b.append("searchObj = lastSearch;\n\n");
			b.append("prepareAfterLoad();\n");
			b.append("performAdvancedSearch();\n");
			b.append("return;\n");
			b.append("}\n");
		}

		if (addAdvSearch) {
			b.append("\n");
			b.append("initSearchObject();\n");
		}

		b.append("\n");

		addDebugLog(b, "Tree view initialization finished");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (!rootItem.getChildren().isEmpty() || recursiveStructure) {
			importPackage("org.primefaces.event");
			methodSignature = "void onNodeExpand(NodeExpandEvent event)";

			boolean firstItem = true;

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * @param event\n");
			b.append(" */\n");
			b.append("@SuppressWarnings(\"unchecked\")\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append(SELECTED_NODE_NAME + " = event.getTreeNode();\n");
			b.append("final String nodeType = " + SELECTED_NODE_NAME + ".getType();\n\n");

			if (recursiveStructure) {
				final var nodeTypeName = tree.getDomainObject().getName().toUpperCase() + "_FOLDER_TYPE";

				b.append("if(nodeType.equals(" + nodeTypeName + "))\n");
				b.append("add" + "SubItems" + "ToTree();\n");

				firstItem = false;
			}

			b.append(addNodeExpandFragment(rootItem, firstItem));
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		if (addAdvSearch) {
			methodSignature = "void initSearchObject()";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Initialize search object\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("int itemIndex = -1;\n\n");
			b.append("// Initialize search object\n");
			b.append("searchObj = new SearchDTO();\n");
			b.append("searchObj.setMaxResult(1000);\n");
			b.append("searchObj.setExactFilterMatch(true);\n");
			b.append("searchObj.setCaseSensitive(false);\n");
			b.append("searchObj.setCount(false);\n\n");
			b.append("refreshFormatSettings(searchObj);\n\n");

			// Add all advanced search input fields
			for (final TreeSearchItem treeItem : tree.getAdvancedSearchItems()) {
				final DomainAttribute attribute = treeItem.getDTOAttribute().getDomainAttribute();
				final var key = treeItem.getDTOAttribute().getDTOBean().getName() + "_asi_" + treeItem.getDTOAttribute().getName();

				b.append("new JSFSearchFieldDTO(searchObj, ++itemIndex, ");
				b.append(treeItem.getDTOAttribute().getSelectTokenConstant());
				b.append(", ");
				b.append(i18n.getBundleFragment(key, treeItem.getLabel()));
				b.append(", ");
				b.append(attribute.getSearchFieldDataType());
				b.append(", 0");

				if (attribute.getTemporalType() == TemporalTypeEnumeration.DATE || attribute.getJavaType().isLocalDate())
					b.append(", false");

				b.append(");\n");
			}

			b.append("\n");
			b.append("visibleFields = new DualListModel<>();\n");
			b.append("visibleFields.setSource(new ArrayList<>());\n");
			b.append("visibleFields.setTarget(searchObj.getSearchFields());\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());

			methodSignature = "void countRecords()";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Perform count operation\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");

			addDebugLog(b, "Perform count operation");

			b.append("\n");
			b.append("try\n");
			b.append("{\n");
			b.append("preSearch();\n");
			b.append("}\n");
			b.append("catch (final SearchInputFieldValidationException e)\n");
			b.append("{\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, ");
			b.append("SEARCH_INPUT_VALIDATION, \"\", e.getSearchFieldName());\n");
			b.append("return;\n");
			b.append("}\n\n");
			b.append("refreshFormatSettings(searchObj);\n\n");
			b.append("try\n");
			b.append("{\n");
			b.append("countResult = ");

			new ServiceInvocationGenerator(tree.getCountMethod(), b).addInvocation("searchObj");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, OPERATION_COUNT_RESULT, \"\", countResult);\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while performing count operation!", "e");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_COUNT_FAIL, e);\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("postSearch();\n");
			b.append("}\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());

			// Add translation methods for enumeration fields
			for (final TreeSearchItem treeItem : tree.getAdvancedSearchItems()) {
				final DTOBeanAttribute attr = treeItem.getDTOAttribute();

				if (!attr.getDomainAttribute().getJavaType().isEnum())
					continue;

				final var transMethodName = "translate" + attr.getUpperCaseName();
				final var javaEnum = (JavaEnum) attr.getDomainAttribute().getJavaType();
				methodSignature = "String " + transMethodName + "(" + javaEnum.getName() + " item)";

				// Generate translations for all literals
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Perform translation of given enumeration literal\n");
				b.append(" * @param item\n");
				b.append(" * @return the translation based on the user's locale\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append("return bundle.getString(\"" + javaEnum.getName().toLowerCase() + "_\" + item.name().toLowerCase());\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}

			// Add auto-complete callback methods for all respective fields
			for (final TreeSearchItem treeItem : tree.getAdvancedSearchItems()) {
				final BoundaryMethod m = getAutoCompleteMethod(treeItem.getDTOAttribute().getDomainAttribute());

				if (m == null)
					continue;

				final DomainAttribute attr = treeItem.getDTOAttribute().getDomainAttribute();
				final var completeMethodName = "onComplete" + attr.getDomainObject().getName() + attr.getUpperCaseName();

				methodSignature = "List<String> " + completeMethodName + "(String query)";

				if (methodSet.contains(methodSignature))
					continue;

				methodSet.add(methodSignature);

				final var listDTO = (DTOBean) m.getReturnType();
				DTOBeanAttribute dtoAttribute = listDTO.getDisplayAttribute();

				if (dtoAttribute == null)
					dtoAttribute = listDTO.getPKAttribute();

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Callback method for auto-complete field\n");
				b.append(" * @param query the filter criterion inserted by the user\n");
				b.append(" * @return a list containing all proposals\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append("final var results = new ArrayList<String>();\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("final Collection<" + listDTO.getModelClassName() + "> items = ");

				new ServiceInvocationGenerator(m, listDTO, b).addInvocation("query + \"%\"");

				b.append("\n");
				b.append("for(final " + listDTO.getModelClassName() + " item : items)\n");
				b.append("results.add(");
				b.append(dtoAttribute.getDomainAttribute().convertToString("item." + dtoAttribute.getModelGetterName()) + ");\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while searching for auto-complete items by using the entered text '{}'!", "e", "query");

				b.append("\n");
				b.append("}\n\n");
				b.append("return results;\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		createDropListener();

		if (recursiveStructure)
			addSubItemTreeBuildMethod(rootItem);

		addMethods(rootItem);

		i18n.save();
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
	 * @param parentItem
	 */
	private void addFields(TreeViewItem parentItem) {
		parentItem.getChildren().forEach(treeItem -> {
			BoundaryMethod method = null;

			if (treeItem.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
				method = getDeleteMethod(treeItem);
			else
				method = getSubItemRemoveMethod(treeItem);

			final EList<BoundaryMethod> downloadMethods = getDownloadMethods(treeItem);

			if (method == null && !downloadMethods.isEmpty())
				method = downloadMethods.get(0);

			if (method != null)
				addInjectedService(method);

			addInjectedService(treeItem.getDataFetchMethod());
			addFields(treeItem);
		});
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
			b.append(i18n.getBundleFragment(key, label) + " + ITEM_LABEL_SEPARATOR);\n");

			if (addNullCheck)
				b.append("\n");
		}

		if (addNullCheck)
			b.append("if(" + getter + " != null)\n");

		b.append("itemText.append(" + attr.getDomainAttribute().convertToString(getter) + ");\n\n");

		return b.toString();
	}

	/**
	 * @param parentItem
	 * @return the generated content
	 */
	private String addSubItemFolderNodes(TreeViewItem parentItem) {
		final var b = new StringBuilder();
		final DTOBeanAttribute pkAttr = parentItem.getItemDTO().getPKAttribute();
		final DomainAttribute domainAttribute = pkAttr.getDomainAttribute();
		int subNodeIndex = 1;

		for (final TreeViewItem item : parentItem.getChildren()) {
			final var nodeName = "subNode" + subNodeIndex;
			final var key = tree.getName() + "_" + item.getAssociation().getName();
			final String label = item.getLabel();
			final var nodeTypeName = item.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			b.append("\n// Add folder node for items of association \"" + item.getAssociation().getName() + "\"\n");
			b.append("final var " + nodeName + " = new DefaultTreeNode<>(" + nodeTypeName + ", ");
			b.append("new TreeNavigatorItem(");
			b.append(domainAttribute.convertToString("item." + pkAttr.getGetterName()));
			b.append(", " + i18n.getBundleFragment(key, label) + ", null), node);\n");
			b.append("new DefaultTreeNode<>(" + NODE_TYPE_DUMMY + ", new TreeNavigatorItem(");
			b.append(NODE_TYPE_DUMMY + "), " + nodeName + ");\n");

			subNodeIndex++;
		}

		// Add a folder for items of a recursive structure
		if (recursiveStructure && parentItem.isRootItem()) {
			final var nodeName = "subNode" + subNodeIndex;
			final var key = tree.getName() + "_" + tree.getDomainObject().getName();
			final String label = tree.getDomainObject().getLabelPlural().substring(0, 1).toUpperCase()
					+ tree.getDomainObject().getLabelPlural().substring(1);
			final var nodeTypeName = tree.getDomainObject().getName().toUpperCase() + "_FOLDER_TYPE";

			b.append("\n// Add folder node for items of recursive structure\n");
			b.append("final var " + nodeName + " = new DefaultTreeNode<>(" + nodeTypeName + ", ");
			b.append("new TreeNavigatorItem(");
			b.append(domainAttribute.convertToString("item." + pkAttr.getGetterName()));
			b.append(", " + i18n.getBundleFragment(key, label) + ", null), node);\n");
			b.append("new DefaultTreeNode<>(" + NODE_TYPE_DUMMY + ", new TreeNavigatorItem(");
			b.append(NODE_TYPE_DUMMY + "), " + nodeName + ");\n");
		}

		return b.toString();
	}

	/**
	 * @param treeItem
	 */
	private void addSubItemTreeBuildMethod(TreeViewItem treeItem) {
		final var b = new StringBuilder();
		String methodSignature;
		BoundaryMethod method = treeItem.getDataFetchMethod();
		DomainAttribute parentPkAttribute;
		boolean recursiveMethod = false;

		if (method == null) {
			method = tree.getRecursiveMethod();
			methodSignature = "void addSubItemsToTree()";
			parentPkAttribute = treeItem.getItemDTO().getPKAttribute().getDomainAttribute();
			recursiveMethod = true;
		}
		else {
			methodSignature = "void add" + treeItem.getAssociation().getUpperCaseName() + "ToTree()";
			parentPkAttribute = treeItem.getParentItem().getItemDTO().getPKAttribute().getDomainAttribute();
		}

		b.append("/**\n");
		b.append(" * Add " + treeItem.getItemDTO().getDomainObject().getLabel() + " objects to parent tree node\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + treeItem.getItemDTO().getDomainObject().getLabel() + " sub-items");

		b.append("\n");
		b.append("final List<" + treeItem.getItemDTO().getName() + "> list;\n");
		b.append("final TreeNavigatorItem parentItem = " + SELECTED_NODE_NAME + ".getData();\n");
		b.append("StringBuilder itemText;\n\n");
		b.append("// Remove all existing nodes!\n");
		b.append(SELECTED_NODE_NAME + ".getChildren().clear();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("list = ");

		new ServiceInvocationGenerator(method, b).addInvocation(parentPkAttribute.convertFromString("parentItem.getId()"));

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while fetching data!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_FETCH_FAIL, e);\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("if(list.isEmpty())\n");
		b.append("{\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, OPERATION_FETCH_NO_DATA);\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("for(final " + treeItem.getItemDTO().getName() + " item : list)\n");
		b.append("{\n");
		b.append(addDisplayAttributes(treeItem));

		final var nodeName = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";
		final DTOBeanAttribute pkAttr = treeItem.getItemDTO().getPKAttribute();
		final DomainAttribute domainAttribute = pkAttr.getDomainAttribute();

		if (!treeItem.getNodes().isEmpty() || !treeItem.getChildren().isEmpty() || recursiveMethod)
			b.append("final var node = ");

		b.append("new DefaultTreeNode<>(" + nodeName + ", ");
		b.append("new TreeNavigatorItem(");
		b.append(domainAttribute.convertToString("item." + pkAttr.getGetterName()));
		b.append(", itemText.toString(), null), " + SELECTED_NODE_NAME + ");\n");
		b.append(addTreeNodes(treeItem.getNodes()));
		b.append(addSubItemFolderNodes(treeItem));
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the method for searching items
	 */
	private void addQuickSearchMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "void performQuickSearch()";

		b.append("/**\n");
		b.append(" * Perform quick-search operation\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("// Initialize search object\n");
		b.append("final var searchInput = new SearchDTO();\n");
		b.append("searchInput.setMaxResult(1000);\n");
		b.append("searchInput.setExactFilterMatch(true);\n");
		b.append("searchInput.setCaseSensitive(false);\n");
		b.append("searchInput.setCount(false);\n\n");

		if (addAdvSearch) {
			b.append("// Disable count operation when performing a quick-search!\n");
			b.append("searchObj.setCount(false);\n\n");
		}

		b.append("refreshFormatSettings(searchInput);\n");

		int itemIndex = 1;

		for (final TreeSearchItem a : tree.getQuickSearchItems()) {
			final var searchFieldName = "field" + itemIndex;
			final var inputFieldName = a.getDTOAttribute().getName() + "Filter";

			b.append("\n");
			b.append("final var " + searchFieldName + " = new SearchFieldDTO(" + (itemIndex - 1) + ", ");
			b.append(a.getDTOAttribute().getSelectTokenConstant() + ", \"\", ");
			b.append("SearchFieldDataTypeEnum.STRING, 0);\n");
			b.append(searchFieldName + ".setFilterCriteria(" + inputFieldName + ");\n");
			b.append("searchInput.getSearchFields().add(" + searchFieldName + ");\n");

			itemIndex++;
		}

		b.append("\nadd" + rootItem.getItemDTO().getDomainObject().getNamePlural() + "ToTree(searchInput);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method for searching tree items
	 */
	private void addAdvancedSearchMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "void performAdvancedSearch()";

		b.append("/**\n");
		b.append(" * Perform advanced search operation\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("preSearch();\n");
		b.append("}\n");
		b.append("catch (final SearchInputFieldValidationException e)\n");
		b.append("{\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, ");
		b.append("SEARCH_INPUT_VALIDATION, \"\", e.getSearchFieldName());\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("refreshFormatSettings(searchObj);\n\n");
		b.append("if(searchObj.isCount())\n");
		b.append("try\n");
		b.append("{\n");
		b.append("countResult = ");

		new ServiceInvocationGenerator(tree.getCountMethod(), b).addInvocation("searchObj");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while performing count operation!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_COUNT_FAIL, e);\n");
		b.append("}\n\n");
		b.append("add" + rootItem.getItemDTO().getDomainObject().getNamePlural() + "ToTree(searchObj);\n\n");
		b.append("postSearch();\n");

		if (saveQueries) {
			b.append("\n");
			b.append(SAVED_QUERY_MANAGER + ".saveQuery(" + USER_SESSION_BEAN + ".getPrincipal().");
			b.append(project.getApplicationLogOnDTO().getPKAttribute().getGetterName());
			b.append(", " + VIEW_ID + ", \"\", searchObj);\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * @param treeItem
	 */
	private void addTreeBuildMethod(TreeViewItem treeItem) {
		final var b = new StringBuilder();

		if (!treeItem.isRootItem()) {
			addSubItemTreeBuildMethod(treeItem);
			return;
		}

		var methodSignature = "void add" + treeItem.getItemDTO().getDomainObject().getNamePlural() + "ToTree(";

		if (addAdvSearch || addQuickSearch)
			methodSignature += "SearchDTO searchInput";

		methodSignature += ")";

		b.append("/**\n");
		b.append(" * Search tree items\n");

		if (addAdvSearch || addQuickSearch)
			b.append(" * @param searchInput\n");

		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("List<" + treeItem.getItemDTO().getName() + "> list;\n");
		b.append("itemCount = 0;\n");
		b.append("StringBuilder itemText;\n\n");

		if (!addAdvSearch && !addQuickSearch && tree.needsSearchObject()) {
			b.append("// Initialize search object\n");
			b.append("final var searchInput = new SearchDTO();\n");
			b.append("searchInput.setMaxResult(1000);\n");
			b.append("searchInput.setExactFilterMatch(true);\n");
			b.append("searchInput.setCaseSensitive(false);\n");
			b.append("searchInput.setCount(false);\n\n");
			b.append("refreshFormatSettings(searchInput);\n\n");
		}

		b.append("// Create root node\n");
		b.append(ROOT_NODE_NAME + " = new DefaultTreeNode<>(\"" + ROOT_NODE_NAME + "\", null);\n\n");

		final BoundaryMethod method = tree.getBoundaryMethod();
		final var invocationGenerator = new ServiceInvocationGenerator(method, b);

		addDebugLog(b, "Perform data fetch operation for " + treeItem.getItemDTO().getDomainObject().getLabel() + " items");

		b.append("\n");
		b.append("try\n");
		b.append("{\n");
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
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_FETCH_FAIL, e);\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("for(final " + treeItem.getItemDTO().getName() + " item : list)\n");
		b.append("{\n");
		b.append(addDisplayAttributes(treeItem));

		final var nodeTypeName = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";
		final DTOBeanAttribute pkAttr = treeItem.getItemDTO().getPKAttribute();
		final DomainAttribute domainAttribute = pkAttr.getDomainAttribute();

		if (!treeItem.getNodes().isEmpty() || !treeItem.getChildren().isEmpty() || recursiveStructure)
			b.append("final var node = ");

		b.append("new DefaultTreeNode<>(" + nodeTypeName + ", ");
		b.append("new TreeNavigatorItem(");
		b.append(domainAttribute.convertToString("item." + pkAttr.getGetterName()));
		b.append(", itemText.toString(), null), " + ROOT_NODE_NAME + ");\n");
		b.append(addTreeNodes(treeItem.getNodes()));
		b.append(addSubItemFolderNodes(treeItem));
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add all nodes of a {@link TreeViewItem} to the tree
	 * @param nodes
	 * @return the generated content
	 */
	private String addTreeNodes(List<TreeNode> nodes) {
		final var b = new StringBuilder();

		nodes.forEach(node -> {
			final DTOBeanAttribute attr = node.getDTOAttribute();

			b.append("\n// Add the \"" + node.getLabel() + "\" tree node\n");

			if (attr.getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE) {
				final var parentNodeName = "node" + attr.getUpperCaseName();
				final var translationKey = attr.getDTOBean().getName() + "_" + attr.getName();

				b.append("final var " + parentNodeName + " = ");
				b.append("new DefaultTreeNode<>(" + NODE_TYPE_DATA + ", new TreeNavigatorItem(");
				b.append(i18n.getBundleFragment(translationKey, node.getLabel()) + "), node);\n\n");
				b.append("for(final var element : item." + attr.getModelGetterName() + ")\n");
				b.append("new DefaultTreeNode<>(" + NODE_TYPE_DATA + ", new TreeNavigatorItem(");
				b.append(node.getDTOAttribute().getDomainAttribute().convertToString("element") + "), " + parentNodeName + ");\n");
			}
			else {
				b.append("itemText = new StringBuilder();\n");
				b.append(createItemText(attr, false, node.getLabel()));
				b.append("new DefaultTreeNode<>(" + NODE_TYPE_DATA + ", new TreeNavigatorItem(itemText.toString()), node);\n");
			}
		});

		return b.toString();
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
	 * @param downloadMethod
	 */
	private void addDownloadMethod(BoundaryMethod downloadMethod) {
		final var b = new StringBuilder();
		final DomainAttribute attr = downloadMethod.getDomainAttribute();
		final var declaration = "StreamedContent " + attr.getGetterName();
		final DomainAttribute pkAttr = downloadMethod.getBoundaryBean().getDomainObject().getPKAttribute();

		b.append("/**\n");
		b.append(" * @return the download stream that contains the file content\n");
		b.append(" */\n");
		b.append("@SuppressWarnings(\"resource\")\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + declaration + "\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(" + SELECTED_NODE_NAME + " == null)\n");
		b.append("return null;\n\n");
		b.append("final TreeNavigatorItem selectedItem = " + SELECTED_NODE_NAME + ".getData();\n");
		b.append("final String path = ");

		new ServiceInvocationGenerator(downloadMethod, b).addInvocation(pkAttr.convertFromString("selectedItem.getId()"));

		b.append("\n");
		b.append("if(path == null || path.isEmpty())\n");
		b.append("return null;\n\n");
		b.append("final String fileName = new File(path).getName();\n");
		b.append("final var inputStream = new FileInputStream(path);\n\n");
		b.append("return DefaultStreamedContent.builder().name(fileName).stream(() -> inputStream).build();\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while performing download operation!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_DOWNLOAD_FAIL, e);\n");
		b.append("return null;\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(declaration, b.toString());
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

			b.append("(nodeType.equals(" + nodeTypeName + "))\n");
			b.append("add" + treeItem.getAssociation().getUpperCaseName() + "ToTree();\n");
			b.append(addNodeExpandFragment(treeItem, false));
		}

		return b.toString();
	}

	/**
	 * @param targetForm
	 * @return the generated content
	 */
	private String getNavigationTargetForForm(Form targetForm) {
		final var b = new StringBuilder();
		final FormTypeEnumeration targetFormType = targetForm.getFormType();

		importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

		if (targetFormType != FormTypeEnumeration.CREATE) {
			b.append("if(" + SELECTED_NODE_NAME + " == null)\n");
			b.append("return \"\";\n\n");
			b.append("final TreeNavigatorItem selectedItem = " + SELECTED_NODE_NAME + ".getData();\n\n");
			b.append("return " + targetForm.getName() + ".PAGE_INIT_URL + ");
			b.append("java.net.URLEncoder.encode(");
			b.append("selectedItem.getId(), java.nio.charset.StandardCharsets.UTF_8);\n");
		}
		else
			b.append("return " + targetForm.getName() + ".PAGE_INIT_URL;\n");

		return b.toString();
	}

	/**
	 * @param treeItem
	 */
	private void addMethods(TreeViewItem treeItem) {
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
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append("final TreeNavigatorItem selectedItem = " + SELECTED_NODE_NAME + ".getData();\n\n");
				b.append("try\n");
				b.append("{\n");

				addDebugLog(b, "Delete selected object with id '{}'", "selectedItem.getId()");

				b.append("\n");

				new ServiceInvocationGenerator(method, b).addInvocation(pkAttr.convertFromString("selectedItem.getId()"));

				b.append("\n");
				b.append("// Remove selected node from tree\n");
				b.append("selectedNode.getChildren().clear();\n");
				b.append("selectedNode.getParent().getChildren().remove(selectedNode);\n");
				b.append("selectedNode.setParent(null);\n");
				b.append("selectedNode = null;\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while deleting selected object!", "e");

				b.append("\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_DELETE_FAIL, e);\n");
				b.append("}\n");
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
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append("final TreeNavigatorItem selectedItem = " + SELECTED_NODE_NAME + ".getData();\n");
				b.append("final TreeNavigatorItem parentItem = " + SELECTED_NODE_NAME + ".getParent().getData();\n\n");
				b.append("try\n");
				b.append("{\n");

				final var logMsg = "Remove selected object with id '{}' from list '" + assocName + "' of parent object with id '{}'";

				addDebugLog(b, logMsg, "selectedItem.getId()", "parentItem.getId()");

				b.append("\n");

				params.add(pkParentAttr.convertFromString("parentItem.getId()"));
				params.add(pkAttr.convertFromString("selectedItem.getId()"));

				new ServiceInvocationGenerator(method, b).addInvocation(params.stream().toArray(String[]::new));

				b.append("\n");
				b.append("// Remove selected node from tree\n");
				b.append("selectedNode.getChildren().clear();\n");
				b.append("selectedNode.getParent().getChildren().remove(selectedNode);\n");
				b.append("selectedNode.setParent(null);\n");
				b.append("selectedNode = null;\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while removing selected object!", "e");

				b.append("\n");
				b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_DELETE_FAIL, e);\n");
				b.append("}\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (getAddForm(treeItem) != null) {
			final var methodSignature = "String add" + treeItem.getItemDTO().getDomainObject().getName() + "()";
			final Form form = getAddForm(treeItem);

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Add " + treeItem.getItemDTO().getDomainObject().getLabel() + "\n");
				b.append(" * @return the navigation target\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append(getNavigationTargetForForm(form));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (getEditForm(treeItem) != null) {
			final var methodSignature = "String edit" + treeItem.getItemDTO().getDomainObject().getName() + "()";
			final Form form = getEditForm(treeItem);

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Edit " + treeItem.getItemDTO().getDomainObject().getLabel() + "\n");
				b.append(" * @return the navigation target\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append(getNavigationTargetForForm(form));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (getCreateNewForm(treeItem) != null && treeItem.isRootItem()) {
			final var methodSignature = "String create" + treeItem.getItemDTO().getDomainObject().getName() + "()";
			final Form form = getCreateNewForm(treeItem);

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Create new " + treeItem.getItemDTO().getDomainObject().getLabel() + "\n");
				b.append(" * @return the navigation target\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append(getNavigationTargetForForm(form));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (getReadOnlyForm(treeItem) != null) {
			final var methodSignature = "String view" + treeItem.getItemDTO().getDomainObject().getName() + "()";
			final Form form = getReadOnlyForm(treeItem);

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * View " + treeItem.getItemDTO().getDomainObject().getLabel() + "\n");
				b.append(" * @return the navigation target\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + methodSignature + "\n");
				b.append("{\n");
				b.append(getNavigationTargetForForm(form));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		getDownloadMethods(treeItem).forEach(this::addDownloadMethod);

		treeItem.getChildren().forEach(this::addMethods);
	}

	/**
	 * Create the drop listener
	 */
	private void createDropListener() {
		final var b = new StringBuilder();
		final var methodSignature = "void onDragDrop(TreeDragDropEvent event)";
		boolean isFirstItem = true;

		if (dropItems.isEmpty())
			return;

		importPackage("org.primefaces.event");

		b.append("/**\n");
		b.append(" * Callback listener for tree item drop events\n");
		b.append(" * @param event\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final TreeNode<?> dragNode = event.getDragNode();\n");
		b.append("final TreeNode<?> dropNode = event.getDropNode();\n");
		b.append("final String dragNodeType = dragNode.getType();\n");
		b.append("final String dropNodeType = dropNode.getType();\n");
		b.append("boolean error = false;\n");
		b.append("boolean isValid = false;\n\n");

		for (final TreeViewItem item : dropItems) {
			final var nodeTypeName = item.getItemDTO().getName().toUpperCase() + "_TYPE";
			final BoundaryMethod dropMethod = item.getDropMethod();
			final DomainAttribute dragPkAttr = item.getItemDTO().getDomainObject().getPKAttribute();
			final String dragIdGetter = dragPkAttr.convertFromString("dragItem.getId()");
			var nodeParentTypeName = item.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";
			DomainAttribute dropPKAttr = dragPkAttr;

			if (item.getAssociation() != null)
				dropPKAttr = item.getAssociation().getDomainObject().getPKAttribute();
			else
				nodeParentTypeName = tree.getDomainObject().getName().toUpperCase() + "_FOLDER_TYPE";

			if (isFirstItem) {
				b.append("\nif");
				isFirstItem = false;
			}
			else
				b.append("else if");

			b.append("(dragNodeType.equals(" + nodeTypeName + ") && dropNodeType.equals(" + nodeParentTypeName + "))\n");
			b.append("{\n");

			final var s = new StringBuilder();
			final String dropIdGetter = dropPKAttr.convertFromString("dropItem.getId()");

			s.append("try\n");
			s.append("{\n");
			s.append("final var dragItem = (TreeNavigatorItem) dragNode.getData();\n");
			s.append("final var dropItem = (TreeNavigatorItem) dropNode.getData();\n\n");

			if (item.getAssociation() == null) {
				s.append("// Avoid cyclic reference!\n");
				s.append("if(!dragItem.getId().equals(dropItem.getId()))\n");
				s.append("{\n");
			}

			if (item.getAssociation() == null || item.getAssociation() instanceof OneToManyAssociation)
				new ServiceInvocationGenerator(dropMethod, s).addInvocation(dragIdGetter, dropIdGetter);
			else
				new ServiceInvocationGenerator(dropMethod, s).addInvocation(dropIdGetter, dragIdGetter);

			s.append("isValid = true;\n");

			if (item.getAssociation() == null)
				s.append("}\n");

			s.append("}\n");
			s.append("catch (final Exception e)\n");
			s.append("{\n");

			addErrorLog(s, "Drop operation failed!", "e");

			s.append("\n");
			s.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_SAVE_FAIL, e);\n");
			s.append("error = true;\n");
			s.append("}\n");

			if (addSecurity && dropMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(dropMethod.getRoles(), s.toString()));
			else
				b.append(s);

			b.append("}\n");
		}

		b.append("\n");
		b.append("if(!isValid)\n");
		b.append("{\n");
		b.append("dropNode.getChildren().remove(dragNode);\n");
		b.append("dragNode.setParent(null);\n\n");
		b.append("if(!error)\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, OPERATION_DRAG_INVALID);\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the search input dialog
	 * @return the generated content
	 */
	private String addAdvancedSearchInputDialog() {
		final var b = new StringBuilder();
		b.append("\t<p:dialog height=\"500\" width=\"1000\" header=\"#{" + EL_I18N_VAR);
		b.append(".search_input_dlg}\" widgetVar=\"dlgSearchInput\" modal=\"true\">\n");
		b.append("\t\t<h:panelGrid columns=\"8\" cellpadding=\"3\">\n");
		b.append("\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_case}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t<p:selectBooleanCheckbox value=\"#{" + managedBeanName + ".searchObject.caseSensitive}\"/>\n");
		b.append("\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_count}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t<p:selectBooleanCheckbox value=\"#{" + managedBeanName + ".searchObject.count}\"/>\n");
		b.append("\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_filter_match}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t<p:selectBooleanCheckbox value=\"#{" + managedBeanName + ".searchObject.exactFilterMatch}\"/>\n");
		b.append("\t\t\t<h:outputLabel value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_filter_fetch_size}:\" styleClass=\"label-field-mandatory\"/>\n");
		b.append("\t\t\t<p:inputText value=\"#{" + managedBeanName + ".searchObject.maxResult}\" size=\"5\">\n");
		b.append("\t\t\t\t<f:validateLongRange minimum=\"1\" maximum=\"1000000\"/>\n");
		b.append("\t\t\t</p:inputText>\n");
		b.append("\t\t</h:panelGrid>\n\n");
		b.append("\t\t<p/>\n\n");
		b.append("\t\t<p:scrollPanel style=\"width:100%; height:400px;\" mode=\"native\">\n");
		b.append("\t\t<h:panelGrid columns=\"5\" cellpadding=\"3\">\n");
		b.append("\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_column}\"/>\n");
		b.append("\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_operator}\"/>\n");
		b.append("\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_sort}\"/>\n");
		b.append("\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_filter_input}\"/>\n");
		b.append("\t\t\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".search_input_and}\"/>\n\n");

		int colIndex = 0;

		// Add all advanced search input fields
		for (final TreeSearchItem treeItem : tree.getAdvancedSearchItems()) {
			final JavaType fieldType = treeItem.getDTOAttribute().getDomainAttribute().getJavaType();
			boolean intType = false;
			boolean floatType = false;
			boolean booleanType = false;
			boolean stringType = false;
			boolean decimalType = false;
			boolean dateType = false;
			boolean enumType = false;

			b.append("\t\t\t<h:outputLabel styleClass=\"label-field-mandatory\" for=\"f_" + colIndex + "\" ");
			b.append("value=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex + "].colLabel}\"/>\n");
			b.append("\t\t\t<p:selectOneMenu valueChangeListener=\"#{" + managedBeanName);
			b.append(".onOperatorChanged}\" style=\"width:150px;\" ");
			b.append("styleClass=\"label-field-optional\" id=\"o_" + colIndex + "\" ");
			b.append("value=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex + "].operator}\" ");
			b.append("converter=\"net.codecadenza.runtime.webclient.primefaces.converter.SearchOperatorDTOConverter\">\n");
			b.append("\t\t\t\t<f:selectItems update=\"b_" + colIndex + "\" value=\"#{" + managedBeanName + ".");

			if (fieldType.isIntegerOrLong()) {
				b.append("numberOperators");
				intType = true;
			}
			else if (fieldType.isFloat() || fieldType.isDouble()) {
				b.append("numberOperators");
				floatType = true;
			}
			else if (fieldType.isBigDecimal()) {
				b.append("numberOperators");
				decimalType = true;
			}
			else if (fieldType.isBoolean()) {
				b.append("boolOperators");
				booleanType = true;
			}
			else if (fieldType.isTemporalType()) {
				b.append("dateOperators");
				dateType = true;
			}
			else if (fieldType.isType(JavaType.STRING, JavaType.CHAR)) {
				b.append("textOperators");
				stringType = true;
			}
			else if (fieldType.isUUID()) {
				stringType = true;

				if (treeItem.getDTOAttribute().getDomainAttribute().isWildcardFilteringSupported())
					b.append("uuIDOperatorsWithLike");
				else
					b.append("uuIDOperators");
			}
			else {
				b.append("enumOperators");
				enumType = true;
			}

			final String inputFieldId;

			if (enumType || booleanType)
				inputFieldId = "fc_" + colIndex;
			else
				inputFieldId = "fi_" + colIndex;

			b.append("}\" ");
			b.append("var=\"advInputItem" + colIndex + "\" itemValue=\"#{advInputItem" + colIndex);
			b.append("}\" itemLabel=\"#{advInputItem" + colIndex + ".description}\"/>\n");
			b.append("\t\t\t\t<f:ajax render=\"p_" + colIndex + "\"/>\n");
			b.append("\t\t\t</p:selectOneMenu>\n");
			b.append("\t\t\t<p:selectOneMenu styleClass=\"label-field-optional\" widgetVar=\"st_" + colIndex + "\" ");
			b.append("id=\"s_" + colIndex + "\" value=\"#{" + managedBeanName);
			b.append(".searchObject.searchFields[" + colIndex + "].sortOrder}\">\n");
			b.append("\t\t\t\t<f:selectItems value=\"#{" + managedBeanName + ".sortOrderList}\"/>\n");
			b.append("\t\t\t</p:selectOneMenu>\n");

			if (intType) {
				b.append("\t\t\t<p:inputText id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].integerCriterion}\" size=\"30\"/>\n");
				b.append("\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
				b.append("\t\t\t\t<p:inputText id=\"b_" + colIndex + "\" rendered=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].between}\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].integerBetweenCriterion}\" size=\"30\"/>\n");
				b.append("\t\t\t</h:panelGroup>\n");
			}
			else if (floatType) {
				b.append("\t\t\t<p:inputText id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].doubleCriterion}\" size=\"30\">\n");
				b.append("\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				b.append("\t\t\t</p:inputText>\n");
				b.append("\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
				b.append("\t\t\t\t<p:inputText id=\"b_" + colIndex + "\" rendered=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].between}\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].doubleBetweenCriterion}\" size=\"30\">\n");
				b.append("\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				b.append("\t\t\t</p:inputText>\n");
				b.append("\t\t\t\t</h:panelGroup>\n");
			}
			else if (decimalType) {
				b.append("\t\t\t<p:inputText id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].bigDecimalCriterion}\" size=\"30\">\n");
				b.append("\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				b.append("\t\t\t</p:inputText>\n");
				b.append("\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
				b.append("\t\t\t\t<p:inputText id=\"b_" + colIndex + "\" rendered=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].between}\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].bigDecimalBetweenCriterion}\" size=\"30\">\n");
				b.append("\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				b.append("\t\t\t</p:inputText>\n");
				b.append("\t\t\t\t</h:panelGroup>\n");
			}
			else if (booleanType) {
				b.append("\t\t\t<p:selectOneMenu id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].stringCriterion}\">\n");
				b.append("\t\t\t\t<f:selectItem itemLabel=\"\" itemValue=\"\"/>\n");
				b.append("\t\t\t\t<f:selectItem itemLabel=\"true\" itemValue=\"true\"/>\n");
				b.append("\t\t\t\t<f:selectItem itemLabel=\"false\" itemValue=\"false\"/>\n");
				b.append("\t\t\t</p:selectOneMenu>\n");
				b.append("\t\t\t<h:panelGroup id=\"p_" + colIndex + "\"/>\n");
			}
			else if (dateType) {
				final TemporalTypeEnumeration temporalType = treeItem.getDTOAttribute().getDomainAttribute().getTemporalType();
				final var showTime = temporalType == TemporalTypeEnumeration.TIMESTAMP || fieldType.isLocalDateTime();
				var dateFormat = "dateTimeFormat";

				if (temporalType == TemporalTypeEnumeration.DATE || fieldType.isLocalDate())
					dateFormat = "dateFormat";

				b.append("\t\t\t<p:datePicker id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].dateCriterion}\" ");
				b.append("pattern=\"#{" + USER_SESSION_BEAN + "." + dateFormat);
				b.append("}\" size=\"20\" showButtonBar=\"true\" showIcon=\"true\"");

				if (showTime)
					b.append(" showTime=\"true\"");

				b.append("/>\n");
				b.append("\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
				b.append("\t\t\t\t<p:datePicker id=\"fb_" + colIndex + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].dateBetweenCriterion}\" ");
				b.append("pattern=\"#{" + USER_SESSION_BEAN + "." + dateFormat);
				b.append("}\" size=\"20\" showButtonBar=\"true\" showIcon=\"true\" ");
				b.append("rendered=\"#{" + managedBeanName + ".searchObject.searchFields[" + colIndex + "].between}\"");

				if (showTime)
					b.append(" showTime=\"true\"");

				b.append("/>\n");
				b.append("\t\t\t</h:panelGroup>\n");
			}
			else if (stringType) {
				final BoundaryMethod m = getAutoCompleteMethod(treeItem.getDTOAttribute().getDomainAttribute());

				if (m != null) {
					final DomainAttribute attr = treeItem.getDTOAttribute().getDomainAttribute();
					final var completeMethodName = "onComplete" + attr.getDomainObject().getName() + attr.getUpperCaseName();

					b.append("\t\t\t<p:autoComplete minQueryLength=\"2\" maxResults=\"10\" completeMethod=\"#{" + managedBeanName + ".");
					b.append(completeMethodName + "}\" ");
					b.append("id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
					b.append(".searchObject.searchFields[" + colIndex + "].stringCriterion}\" size=\"30\"/>\n");
					b.append("\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
					b.append("\t\t\t<p:autoComplete minQueryLength=\"2\" rendered=\"#{" + managedBeanName);
					b.append(".searchObject.searchFields[" + colIndex + "].between}\" maxResults=\"10\" completeMethod=\"#{");
					b.append(managedBeanName + "." + completeMethodName + "}\" ");
					b.append("id=\"b_" + colIndex + "\" value=\"#{" + managedBeanName);
				}
				else {
					b.append("\t\t\t<p:inputText id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
					b.append(".searchObject.searchFields[" + colIndex + "].stringCriterion}\" size=\"30\"/>\n");
					b.append("\t\t\t<h:panelGroup id=\"p_" + colIndex + "\">\n");
					b.append("\t\t\t\t<p:inputText id=\"b_" + colIndex + "\" rendered=\"#{" + managedBeanName);
					b.append(".searchObject.searchFields[" + colIndex + "].between}\" value=\"#{" + managedBeanName);
				}

				b.append(".searchObject.searchFields[" + colIndex + "].stringBetweenCriterion}\" size=\"30\"/>\n");
				b.append("\t\t\t</h:panelGroup>\n");
			}
			else if (enumType) {
				final var javaEnum = (JavaEnum) treeItem.getDTOAttribute().getDomainAttribute().getJavaType();

				b.append("\t\t\t<p:selectOneMenu id=\"" + inputFieldId + "\" value=\"#{" + managedBeanName);
				b.append(".searchObject.searchFields[" + colIndex + "].stringCriterion}\">\n");
				b.append("\t\t\t\t<f:selectItem itemLabel=\"\" itemValue=\"\"/>\n");

				javaEnum.getEnumerationValues().forEach(literal -> {
					b.append("\t\t\t\t<f:selectItem itemLabel=\"#{" + EL_I18N_VAR + "." + javaEnum.getName().toLowerCase());
					b.append("_" + literal.getName().toLowerCase() + "}\" itemValue=\"");
					b.append(literal.getName() + "\"/>\n");
				});

				b.append("\t\t\t</p:selectOneMenu>\n");
				b.append("\t\t\t<h:panelGroup id=\"p_" + colIndex + "\"/>\n");
			}

			b.append("\n");
			colIndex++;
		}

		b.append("\t\t</h:panelGrid>\n");
		b.append("\t\t</p:scrollPanel>\n");
		b.append("\t\t<br/>\n");
		b.append("\t\t<h:panelGrid columns=\"3\">\n");
		b.append("\t\t\t<p:commandButton id=\"cmdSearch\" value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_perform_search}\" action=\"#{" + managedBeanName);
		b.append(".performAdvancedSearch}\" ajax=\"false\"/>\n");
		b.append("\t\t\t<p:commandButton id=\"cmdReset\" value=\"#{" + EL_I18N_VAR);
		b.append(".command_reset}\" action=\"#{" + managedBeanName);
		b.append(".initSearchObject}\" onclick=\"PF('dlgSearchInput').hide()\" ajax=\"false\"/>\n");
		b.append("\t\t\t<p:commandButton id=\"cmdCount\" value=\"#{" + EL_I18N_VAR);
		b.append(".search_input_perform_count}\" actionListener=\"#{");
		b.append(managedBeanName + ".countRecords");
		b.append("}\" update=\"growl\" ajax=\"true\"/>\n");
		b.append("\t\t</h:panelGrid>\n");
		b.append("\t</p:dialog>\n\n");

		return b.toString();
	}

	/**
	 * Add the context menu
	 * @param treeItem
	 * @return the generated content
	 */
	private String addContextMenu(TreeViewItem treeItem) {
		final var b = new StringBuilder();
		BoundaryMethod method = null;
		var nodeType = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";

		b.append("\t<p:contextMenu for=\"tree\" nodeType=\"" + nodeType + "\">\n");

		if (getAddForm(treeItem) != null) {
			final var methodName = "add" + treeItem.getItemDTO().getDomainObject().getName();

			// Add a context menu item to open a form to add a new object
			b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_add}\" ajax=\"true\" ");
			b.append("icon=\"pi pi-plus\"" + securityHelper.addSecurityCode(getAddForm(treeItem).getRoles()));
			b.append("action=\"#{" + managedBeanName + "." + methodName + "}\">\n");
			b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
			b.append(managedBeanName + ".currentPageURL}\"/>\n");
			b.append("\t\t</p:menuitem>\n");
		}

		if (getEditForm(treeItem) != null) {
			final var methodName = "edit" + treeItem.getItemDTO().getDomainObject().getName();

			b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_edit}\" ajax=\"true\" ");
			b.append("icon=\"pi pi-pencil\"" + securityHelper.addSecurityCode(getEditForm(treeItem).getRoles()));
			b.append("action=\"#{" + managedBeanName + "." + methodName + "}\">\n");
			b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
			b.append(managedBeanName + ".currentPageURL}\"/>\n");
			b.append("\t\t</p:menuitem>\n");
		}

		if (getReadOnlyForm(treeItem) != null) {
			final var methodName = "view" + treeItem.getItemDTO().getDomainObject().getName();

			b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_view}\" ajax=\"true\" ");
			b.append("icon=\"pi pi-file\"" + securityHelper.addSecurityCode(getReadOnlyForm(treeItem).getRoles()));
			b.append("action=\"#{" + managedBeanName + "." + methodName + "}\">\n");
			b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
			b.append(managedBeanName + ".currentPageURL}\"/>\n");
			b.append("\t\t</p:menuitem>\n");
		}

		if (getCreateNewForm(treeItem) != null && treeItem.isRootItem()) {
			final var actionMethodName = "create" + rootItem.getItemDTO().getDomainObject().getName();

			// Add a context menu item to open a form to create a new object
			b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_create}\" ajax=\"true\" ");
			b.append("icon=\"pi pi-plus\"" + securityHelper.addSecurityCode(getCreateNewForm(treeItem).getRoles()));
			b.append("action=\"#{" + managedBeanName + "." + actionMethodName + "}\">\n");
			b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
			b.append(managedBeanName + ".currentPageURL}\"/>\n");
			b.append("\t\t</p:menuitem>\n");
		}

		// Add context menu items for file download operations
		getDownloadMethods(treeItem).forEach(downloadMethod -> {
			b.append("\t\t<p:menuitem value=\"#{i18n.action_download}\" ajax=\"false\" icon=\"pi pi-download\"");

			if (addSecurity && downloadMethod.getPermissionMode() == PermissionModeEnumeration.DEDICATED_ROLES)
				b.append(securityHelper.addSecurityCode(downloadMethod.getRoles()));

			b.append(">\n");
			b.append("\t\t\t<p:fileDownload value=\"#{" + managedBeanName + ".");
			b.append(downloadMethod.getDomainAttribute().getName() + "}\"/>\n");
			b.append("\t\t</p:menuitem>\n");
		});

		if (treeItem.isRootItem() || treeItem.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
			method = getDeleteMethod(treeItem);
		else
			method = getSubItemRemoveMethod(treeItem);

		if (method != null) {
			var widgetVar = "deleteConfirm";

			if (treeItem.getAssociation() == null)
				widgetVar += treeItem.getItemDTO().getDomainObject().getName();
			else
				widgetVar += treeItem.getAssociation().getUpperCaseName();

			b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_delete}\" ajax=\"true\" ");
			b.append("icon=\"pi pi-trash\"");

			if (addSecurity && method.getPermissionMode() == PermissionModeEnumeration.DEDICATED_ROLES)
				b.append(securityHelper.addSecurityCode(method.getRoles()));

			b.append(" onclick=\"PF('" + widgetVar + "').show()\"/>\n");
		}

		b.append("\t</p:contextMenu>\n\n");

		if (!treeItem.isRootItem()) {
			var methodName = "";
			nodeType = treeItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			b.append("\t<p:contextMenu for=\"tree\" nodeType=\"" + nodeType + "\">\n");

			if (getAddForm(treeItem) != null) {
				methodName = "add" + treeItem.getItemDTO().getDomainObject().getName();

				// Add a context menu item to open a dialog to add a new object
				b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_add}\" ajax=\"true\" ");
				b.append("icon=\"pi pi-plus\"" + securityHelper.addSecurityCode(getAddForm(treeItem).getRoles()));
				b.append("action=\"#{" + managedBeanName + "." + methodName + "}\">\n");
				b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
				b.append(managedBeanName + ".currentPageURL}\"/>\n");
				b.append("\t\t</p:menuitem>\n");
			}

			methodName = "add" + treeItem.getAssociation().getUpperCaseName() + "ToTree";

			// Add a context menu item to refresh a sub-tree item
			b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_refresh}\" ajax=\"false\" type=\"push\" ");
			b.append("icon=\"pi pi-refresh\" ");
			b.append("actionListener=\"#{" + managedBeanName + "." + methodName + "}\"/>\n");
			b.append("\t</p:contextMenu>\n\n");
		}

		treeItem.getChildren().forEach(subItem -> b.append(addContextMenu(subItem)));

		return b.toString();
	}

	/**
	 * Add confirmation dialogs for the given tree view item
	 * @param treeItem
	 * @return the generated content
	 */
	private String addConfirmDialogs(TreeViewItem treeItem) {
		final var b = new StringBuilder();
		BoundaryMethod method = null;

		if (treeItem.isRootItem() || treeItem.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
			method = getDeleteMethod(treeItem);
		else
			method = getSubItemRemoveMethod(treeItem);

		if (method != null) {
			final var deleteEL = managedBeanName + "." + method.getName();
			var widgetVar = "deleteConfirm";

			if (treeItem.getAssociation() == null)
				widgetVar += treeItem.getItemDTO().getDomainObject().getName();
			else
				widgetVar += treeItem.getAssociation().getUpperCaseName();

			b.append("\t<p:confirmDialog modal=\"true\" message=\"#{" + EL_I18N_VAR + ".dialog_delete_question}\" header=\"#{");
			b.append(EL_I18N_VAR + ".dialog_delete_title}\" severity=\"alert\" widgetVar=\"" + widgetVar + "\">\n");
			b.append("\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_yes}\" oncomplete=\"PF('" + widgetVar);
			b.append("').hide()\" actionListener=\"#{" + deleteEL + "}\" ajax=\"false\"/>\n");
			b.append("\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_no}\" onclick=\"PF('" + widgetVar);
			b.append("').hide()\" type=\"button\"/>\n");
			b.append("\t</p:confirmDialog>\n\n");
		}

		treeItem.getChildren().forEach(subItem -> b.append(addConfirmDialogs(subItem)));

		return b.toString();
	}

	/**
	 * @param parentTreeItem
	 * @return the generated content
	 */
	private String addNodeTypes(TreeViewItem parentTreeItem) {
		final var b = new StringBuilder();

		parentTreeItem.getChildren().forEach(treeItem -> {
			final var nodeType = treeItem.getItemDTO().getName().toUpperCase() + "_TYPE";

			b.append("\t\t<p:treeNode type=\"" + nodeType + "\" icon=\"pi pi-folder\">\n");
			b.append("\t\t\t<h:outputText value=\"#{treeViewItem.name}\"/>\n");
			b.append("\t\t</p:treeNode>\n\n");

			final var nodeParentType = treeItem.getItemDTO().getName().toUpperCase() + "_FOLDER_TYPE";

			b.append("\t\t<p:treeNode type=\"" + nodeParentType + "\" icon=\"pi pi-folder\">\n");
			b.append("\t\t\t<h:outputText value=\"#{treeViewItem.name}\"/>\n");
			b.append("\t\t</p:treeNode>\n\n");
			b.append(addNodeTypes(treeItem));
		});

		return b.toString();
	}

	/**
	 * Create the XHTML form content
	 * @return the generated content
	 */
	public String createXHTMLForm() {
		final var b = new StringBuilder();
		final var rootNodeType = rootItem.getItemDTO().getName().toUpperCase() + "_TYPE";
		final Form createForm = getCreateNewForm(rootItem);

		b.append(JSFGeneratorUtil.createCompositeHeader());
		b.append("<f:metadata>\n");
		b.append("\t<f:viewAction action=\"#{" + managedBeanName + ".initView()}\"/>\n");
		b.append("</f:metadata>\n\n");
		b.append("<ui:define name=\"title\">#{" + managedBeanName + "." + FORM_TITLE + "}</ui:define>\n\n");
		b.append("<ui:define name=\"content\">\n");
		b.append("\t<p:growl id=\"growl\" showDetail=\"true\"/>\n");
		b.append("\t<h:panelGrid columns=\"3\">\n");
		b.append("\t\t<div class=\"pi pi-list\" style=\"font-size: 2em\"/>\n");

		// Entity &#160; is equal to &nbsp;!
		b.append("\t\t<h:outputText id=\"lblFormTitle\" value=\"&#160;#{" + managedBeanName);
		b.append("." + FORM_TITLE + "}\" styleClass=\"label-form-title\"/>\n");

		b.append("\t\t<p:ajaxStatus style=\"width:16px;height:16px;\">\n");
		b.append("\t\t\t<f:facet name=\"start\">\n");
		b.append("\t\t\t\t<h:graphicImage value=\"/images/ajaxloading.gif\"/>\n");
		b.append("\t\t\t</f:facet>\n\n");
		b.append("\t\t\t<f:facet name=\"complete\">\n");
		b.append("\t\t\t\t<h:outputText value=\"\"/>\n");
		b.append("\t\t\t</f:facet>\n");
		b.append("\t\t</p:ajaxStatus>\n");
		b.append("\t</h:panelGrid>\n\n");

		if (addAdvSearch || createForm != null) {
			b.append("\t<p:menubar>\n");

			if (addAdvSearch) {
				b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_search_input}\" ajax=\"true\" ");
				b.append("icon=\"pi pi-search\" ");
				b.append("onclick=\"PF('dlgSearchInput').show()\"/>\n");
			}

			if (createForm != null) {
				final var actionMethodName = "create" + rootItem.getItemDTO().getDomainObject().getName();

				// Add a context menu item to open a form to create a new object
				b.append("\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".action_create}\" ajax=\"true\" ");
				b.append("icon=\"pi pi-plus\"" + securityHelper.addSecurityCode(createForm.getRoles()));
				b.append("action=\"#{" + managedBeanName + "." + actionMethodName + "}\">\n");
				b.append("\t\t\t<f:setPropertyActionListener target=\"#{userSession.lastPage}\" value=\"#{");
				b.append(managedBeanName + ".currentPageURL}\"/>\n");
				b.append("\t\t</p:menuitem>\n");
			}

			b.append("\t</p:menubar>\n\n");
			b.append("\t<p/>\n\n");
		}

		if (addQuickSearch) {
			b.append("\t<p:panel>\n");
			b.append("\t\t<h:panelGrid columns=\"2\">\n");

			tree.getQuickSearchItems().forEach(treeItem -> {
				final var inputFieldName = treeItem.getDTOAttribute().getName() + "Filter";
				final var key = treeItem.getDTOAttribute().getDTOBean().getName() + "_qsi_" + treeItem.getDTOAttribute().getName();
				final String label = treeItem.getLabel();

				b.append("\t\t\t<h:outputLabel value=\"" + i18n.getI18NMessage(key, label) + ":\" for=\"");
				b.append(inputFieldName + "\" styleClass=\"label-field-mandatory\"/>\n");
				b.append("\t\t\t<p:inputText id=\"" + inputFieldName + "\" value=\"#{");
				b.append(managedBeanName + "." + inputFieldName + "}\"/>\n\n");
			});

			b.append("\t\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".search_input_perform_search}\" ");
			b.append("action=\"#{" + managedBeanName + ".performQuickSearch}\" ajax=\"false\"/>\n");
			b.append("\t\t</h:panelGrid>\n");
			b.append("\t</p:panel>\n\n");
			b.append("\t<p/>\n\n");
		}

		if (addAdvSearch)
			b.append(addAdvancedSearchInputDialog());

		b.append("\t<p:tree style=\"width:100%;\" id=\"tree\" value=\"#{" + managedBeanName + "." + ROOT_NODE_NAME + "}\" ");
		b.append("selection=\"#{" + managedBeanName + "." + SELECTED_NODE_NAME);
		b.append("}\" var=\"treeViewItem\" dynamic=\"true\" selectionMode=\"single\"");

		if (!dropItems.isEmpty())
			b.append(" draggable=\"true\" droppable=\"true\"");

		b.append(">\n");

		if (!rootItem.getChildren().isEmpty() || recursiveStructure)
			b.append("\t\t<p:ajax event=\"expand\" listener=\"#{" + managedBeanName + ".onNodeExpand}\" update=\":form:growl\"/>\n");

		if (!dropItems.isEmpty()) {
			b.append("\t\t<p:ajax event=\"dragdrop\" listener=\"#{" + managedBeanName);
			b.append(".onDragDrop}\" update=\":form:growl, :form:tree\"/>\n");
		}

		b.append("\n\t\t<p:treeNode type=\"" + rootNodeType + "\" icon=\"pi pi-folder\">\n");
		b.append("\t\t\t<h:outputText value=\"#{treeViewItem.name}\"/>\n");
		b.append("\t\t</p:treeNode>\n\n");

		if (recursiveStructure) {
			final var nodeType = tree.getDomainObject().getName().toUpperCase() + "_FOLDER_TYPE";

			b.append("\t\t<p:treeNode type=\"" + nodeType + "\" icon=\"pi pi-folder\">\n");
			b.append("\t\t\t<h:outputText value=\"#{treeViewItem.name}\"/>\n");
			b.append("\t\t</p:treeNode>\n\n");
		}

		b.append(addNodeTypes(rootItem));
		b.append("\t\t<p:treeNode type=\"" + NODE_TYPE_DUMMY + "\">\n");
		b.append("\t\t\t<h:outputText value=\"#{treeViewItem.name}\"/>\n");
		b.append("\t\t</p:treeNode>\n\n");
		b.append("\t\t<p:treeNode type=\"" + NODE_TYPE_DATA + "\" icon=\"pi pi-file\">\n");
		b.append("\t\t\t<h:outputText value=\"#{treeViewItem.name}\"/>\n");
		b.append("\t\t</p:treeNode>\n\n");
		b.append("\t</p:tree>\n\n");
		b.append("\t<h:outputText styleClass=\"label-field-mandatory\" value=\"#{" + EL_I18N_VAR);
		b.append(".result_total_number_records} #{" + managedBeanName + ".itemCount}\"/>\n");

		if (addAdvSearch) {
			b.append("\t<h:outputText styleClass=\"label-field-mandatory\" rendered=\"#{" + managedBeanName);
			b.append(".searchObject.count}\" value=\"&#160;#{" + EL_I18N_VAR + ".search_input_count_of}&#160;\"/>\n");
			b.append("\t<h:outputText styleClass=\"label-field-mandatory\" rendered=\"#{" + managedBeanName);
			b.append(".searchObject.count}\" value=\"#{" + managedBeanName + ".countResult}\"/>\n");
		}

		b.append("\n");
		b.append(addContextMenu(rootItem));
		b.append(addConfirmDialogs(rootItem));
		b.append("</ui:define>\n");
		b.append("</ui:composition>\n");

		i18n.save();

		return b.toString();
	}

}
