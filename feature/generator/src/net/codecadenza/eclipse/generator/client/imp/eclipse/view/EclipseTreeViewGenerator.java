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
package net.codecadenza.eclipse.generator.client.imp.eclipse.view;

import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getAddForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getCreateNewForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDeleteMethod;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDownloadMethod;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getDownloadMethods;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getEditForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getReadOnlyForm;
import static net.codecadenza.eclipse.generator.client.common.tree.TreeViewHelper.getSubItemRemoveMethod;
import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;

import java.util.Collection;
import java.util.Map;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.file.EclipseFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.eclipse.util.EclipseClientFieldHelper;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.TreeSearchItem;
import net.codecadenza.eclipse.model.client.TreeView;
import net.codecadenza.eclipse.model.client.TreeViewItem;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for tree views of an Eclipse RCP/RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseTreeViewGenerator extends AbstractTreeViewGenerator {
	private final RichClientI18NGenerator i18n;
	private final EclipseSecurityHelper securityHelper;

	/**
	 * Constructor
	 * @param tree
	 */
	public EclipseTreeViewGenerator(TreeView tree) {
		super(tree);

		this.securityHelper = new EclipseSecurityHelper(project);
		this.addSecurity = securityHelper.isSecurityAdded();
		this.i18n = new RichClientI18NGenerator(project);

		initializeInternalFields();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage("java.text");
		importPackage("java.util");
		importClass("jakarta.annotation.PostConstruct");
		importClass("jakarta.annotation.PreDestroy");
		importPackage("org.eclipse.jface.action");
		importClass("org.eclipse.jface.dialogs.MessageDialog");
		importClass("org.eclipse.swt.SWT");
		importClass("org.eclipse.swt.layout.GridData");
		importClass("org.eclipse.swt.layout.GridLayout");
		importPackage("org.eclipse.swt.widgets");
		importPackage("net.codecadenza.runtime.richclient.format");
		importPackage("net.codecadenza.runtime.richclient.eclipse.image");
		importPackage("org.eclipse.core.runtime.jobs");
		importPackage("org.eclipse.core.runtime");

		if (project.hasRAPClient())
			importClass("net.codecadenza.runtime.richclient.eclipse.rap.services.FormatPreferencesManager");

		if (!dropItems.isEmpty())
			importPackage("org.eclipse.swt.dnd");

		addImports(securityHelper.getSecurityImports());

		if (!dropItems.isEmpty())
			importClass("org.eclipse.swt.graphics.Point");

		if (tree.needsSearchObject())
			importPackage("net.codecadenza.runtime.search.dto");

		if (addAdvSearch) {
			importPackage("net.codecadenza.runtime.richclient.eclipse.search");
			importPackage("net.codecadenza.runtime.richclient.search.util");
		}

		// Add all DTO imports
		importPackage(rootItem.getItemDTO().getNamespace().toString());

		addSubItemDTOImports(rootItem.getChildren());

		addSubItemBoundaryImports(rootItem.getChildren());

		if (needsDateFormatter || needsDateTimeFormatter)
			importPackage(PACK_JAVA_TIME_FORMAT);

		// Check if imports for downloading LOB attributes are necessary
		final EList<BoundaryMethod> downloadMethods = getDownloadMethods(rootItem);

		if (!downloadMethods.isEmpty())
			new EclipseFileHandlingGenerator(this, downloadMethods.stream().findFirst().orElseThrow(), i18n).addImports();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		// Create the class body
		b.append("public class " + tree.getName());

		if (addAdvSearch)
			b.append(" implements Countable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final var viewId = project.getClientNamespace().toString() + PACK_CLIENT_TREE + "." + tree.getName();
		final BoundaryBean boundaryBean = tree.getBoundaryMethod().getBoundaryBean();

		addPublicConstant(JavaType.STRING, "ID", "\"" + viewId + "\"").create();
		addProtectedConstant(JavaType.STRING, "ITEM_LABEL_SEPARATOR", "\": \"").create();

		if (tree.getRecursiveMethod() != null)
			addPrivateConstant(JavaType.STRING, "ITEM_DATA_KEY", "\"ADD_DATA_KEY\"").create();

		addPrivateField("Tree", "tree").create();
		addPrivateField("Label", "lblStateImage").create();
		addPrivateField("Label", "lblResult").create();
		addPrivateField("RefreshAction", "actionRefresh").create();
		addPrivateField("FormatDTO", "format").withDefaultValue("FormatPreferencesManager.getFormatDTO()").withFinalModifier()
				.create();
		addPrivateField("DecimalFormat", "decimalFormat").withDefaultValue("new DecimalFormat(format.getDecimalFormat())")
				.withFinalModifier().create();
		addPrivateField("Shell", "parentShell").create();
		addPrivateField("ToolBarManager", "toolBarManager").create();
		addPrivateField("ToolBar", "toolBar").create();

		new ServiceDeclarationGenerator(this, boundaryBean).addField(false);

		if (needsDateTimeFormatter) {
			final var defaultValue = "DateTimeFormatter.ofPattern(format.getDateTimeFormat()).withZone(" + PACK_JAVA_TIME
					+ ".ZoneId.systemDefault())";

			addPrivateField("DateTimeFormatter", "dateTimeFormat").withDefaultValue(defaultValue).withFinalModifier().create();
		}

		if (needsDateFormatter) {
			final var defaultValue = "DateTimeFormatter.ofPattern(format.getDateFormat()).withZone(" + PACK_JAVA_TIME
					+ ".ZoneId.systemDefault())";

			addPrivateField("DateTimeFormatter", "dateFormat").withDefaultValue(defaultValue).withFinalModifier().create();
		}

		final DomainObject rootDomainObject = rootItem.getItemDTO().getDomainObject();
		final var menuName = "menu" + rootDomainObject.getName();

		addPrivateField("Menu", menuName).create();

		menuSet.add(menuName);

		// Add fields for the context menus
		addMenuFields(rootItem.getChildren());

		if (getCreateNewForm(rootItem) != null) {
			final var actionName = "actionCreate" + rootDomainObject.getName();

			addPrivateField("Create" + rootDomainObject.getName() + "Action", actionName).create();
		}

		if (addAdvSearch) {
			addPrivateField("AdvancedSearchAction", "actionAdvSearch").create();
			addPrivateField(tree.getName(), "thisView").create();
		}

		// Add fields for performing a quick-search!
		if (addQuickSearch) {
			tree.getQuickSearchItems().forEach(a -> {
				final DTOBeanAttribute attr = a.getDTOAttribute();
				final var fieldName = "txt" + attr.getUpperCaseName();

				addPrivateField("Text", fieldName).create();
			});
		}

		// Add a search object if necessary
		if (tree.needsSearchObject())
			addPrivateField("SearchDTO", "searchObj").create();

		if (!dropItems.isEmpty())
			addPrivateField("TreeItem", "dragSourceItem").create();

		addPrivateField("Job", "queryJob").create();
		addPrivateField("java.util.List<" + rootItem.getItemDTO().getName() + ">", "data").create();
		addPrivateField("Display", "display").create();
		addPrivateField("StopSearchAction", "actionStopSearch").create();

		new EclipseClientFieldHelper(tree, this).addClientField();

		// Add helper DTOs
		addHelperDTOs();

		// Add comparators
		addComparatorClasses(rootItem);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();

		// Add the method to perform a quick-search
		if (addQuickSearch)
			addQuickSearchMethod();

		// Add the count method
		if (addAdvSearch)
			addCountMethod();

		// Add the tree item methods
		addTreeMethods();

		// Add methods to initialize the context menus
		addTreeMenuMethods();

		// Add the method to fetch deeper tree levels
		if (recursiveStructure)
			addRecursiveMethod(rootItem);

		// Add methods to initialize the parent context menus
		addAllSubItemMenuMethods(rootItem.getChildren());

		// Add methods to fetch items
		addAllSubItemFetchMethods(rootItem.getChildren());

		addDataFetchJob();

		// Add the method to build the tree
		addTreeBuildMethod();

		// Add actions
		addActions();

		// Add the key listener method
		if (addKeyListener())
			createKeyListener();

		// Add the drag source listener method
		createDragSourceListener();

		// Add the drop target listener method
		createDropListener();

		// Add the tree listener
		if (!helperDTOMap.isEmpty() || tree.getRecursiveMethod() != null)
			createTreeListener(rootItem);

		// Add the mouse listener
		createMouseListener();

		// Add the method to initialize the view
		addPartControlMethod();

		var methodSignature = "TreeItem getSelectedItem()";

		b.append("/**\n");
		b.append(" * @return the selected tree item or null if no item is selected\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("return Arrays.asList(tree.getSelection()).stream().findFirst().orElse(null);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void dispose()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Dispose internal resources\n");
		b.append(" */\n");
		b.append("@PreDestroy\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, tree.getBoundaryMethod(), b);

		if (declarationGenerator.needsCloseStatement()) {
			b.append("if(" + declarationGenerator.getServiceName() + " != null)\n");

			declarationGenerator.addCloseStatement();
		}
		else
			b.append("// No implementation required!\n");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		i18n.save();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator#
	 * addSubItemBoundaryImports(java.util.Collection)
	 */
	@Override
	protected void addSubItemBoundaryImports(Collection<TreeViewItem> items) {
		super.addSubItemBoundaryImports(items);

		items.forEach(item -> {
			// Add imports for download operations
			final EList<BoundaryMethod> downloadMethods = getDownloadMethods(item);

			if (!downloadMethods.isEmpty())
				new EclipseFileHandlingGenerator(this, downloadMethods.stream().findFirst().get(), i18n).addImports();

			if (item.getAssociation() instanceof ManyToManyAssociation)
				importClass("net.codecadenza.runtime.repository.DuplicateCollectionEntryException");

			addSubItemBoundaryImports(item.getChildren());
		});
	}

	/**
	 * Recursive method to add menu properties
	 * @param items
	 */
	private void addMenuFields(Collection<TreeViewItem> items) {
		for (final TreeViewItem item : items) {
			final DomainObject e = item.getItemDTO().getDomainObject();
			var menuName = "menu" + e.getName();

			if (!menuSet.contains(menuName)) {
				addPrivateField("Menu", menuName).create();

				menuSet.add(menuName);
			}

			// Add the context menu for the parent of this item
			final String assocName = item.getAssociation().getUpperCaseName();

			menuName = "menu" + assocName;
			addPrivateField("Menu", menuName).create();

			addMenuFields(item.getChildren());
		}
	}

	/**
	 * Create private helper data transfer objects
	 */
	private void addHelperDTOs() {
		var b = new StringBuilder();
		boolean addAbstractDTOClass = false;

		if (tree.getRecursiveMethod() != null) {
			final DomainObject rootDomainObject = rootItem.getItemDTO().getDomainObject();
			final var dtoName = rootDomainObject.getName() + "TreeHelperDTO";
			final String pkTypeName = rootDomainObject.getPKAttribute().getJavaType().getName();
			addAbstractDTOClass = true;

			b.append("\n/**\n");
			b.append(" * Helper class for tree view sub-items\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private class " + dtoName + " extends AbstractSubItemHelperDTO\n");
			b.append("{\n");
			b.append("private static final long serialVersionUID = 1L;\n\n");
			b.append("public " + dtoName + "(" + pkTypeName + " parentId)\n");
			b.append("{\n");
			b.append("this.parentId = parentId;\n");
			b.append("}\n\n");
			b.append("private final " + pkTypeName + " parentId;\n\n");
			b.append("public " + pkTypeName + " getParentId()\n");
			b.append("{\n");
			b.append("return parentId;\n");
			b.append("}\n");
			b.append("}\n\n");

			addSubClass(dtoName, b.toString());
		}

		addSubItemHelperDTOs(rootItem.getChildren());

		if (addAbstractDTOClass || !subTreeItemDTOMap.isEmpty()) {
			final var dtoName = "AbstractSubItemHelperDTO";

			b = new StringBuilder();
			b.append("\n/**\n");
			b.append(" * Abstract base class for tree view helper data transfer objects\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private abstract class " + dtoName + " implements java.io.Serializable\n");
			b.append("{\n");
			b.append("private static final long serialVersionUID = 1L;\n\n");
			b.append("private boolean dataLoaded;\n\n");
			b.append("public boolean isDataLoaded()\n");
			b.append("{\n");
			b.append("return dataLoaded;\n");
			b.append("}\n\n");
			b.append("public void setDataLoaded(boolean dataLoaded)\n");
			b.append("{\n");
			b.append("this.dataLoaded = dataLoaded;\n");
			b.append("}\n");
			b.append("}\n\n");

			addSubClass(dtoName, b.toString());
		}
	}

	/**
	 * Add the count method
	 */
	private void addCountMethod() {
		final var b = new StringBuilder();
		final BoundaryMethod countMethod = tree.getCountMethod();
		final var methodSignature = "long countData()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.Countable#countData()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return ");

		new ServiceInvocationGenerator(countMethod, b).addInvocation("searchObj");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method to perform a quick-search operation
	 */
	private void addQuickSearchMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "void performQuickSearch()";

		b.append("/**\n");
		b.append(" * Perform quick-search\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		// If the tree view provides an advanced search action the search object must be reinitialized prior to performing a
		// quick-search
		if (addAdvSearch) {
			b.append("// Initialize search object\n");
			b.append("searchObj = new SearchDTO();\n");
			b.append("searchObj.setDateFormat(format.getDateFormat());\n");
			b.append("searchObj.setDateTimeFormat(format.getDateTimeFormat());\n");
			b.append("searchObj.setNumberFormat(format.getDecimalFormat());\n");
			b.append("searchObj.setCaseSensitive(false);\n");
			b.append("searchObj.setExactFilterMatch(true);\n");
			b.append("searchObj.setCount(false);\n");
			b.append("searchObj.setMaxResult(1000);\n\n");
		}
		else
			b.append("searchObj.getSearchFields().clear();\n\n");

		int fieldCount = 1;

		for (final TreeSearchItem a : tree.getQuickSearchItems()) {
			final var fieldName = "txt" + a.getDTOAttribute().getUpperCaseName();
			final var searchFieldName = "field" + fieldCount;

			b.append("if(!" + fieldName + ".getText().isEmpty())\n");
			b.append("{\n");
			b.append("final String filter = " + fieldName + ".getText().endsWith(\"%\") ? " + fieldName);
			b.append(".getText() : " + fieldName + ".getText() + \"%\";\n");
			b.append("final var " + searchFieldName + " = new SearchFieldDTO(");
			b.append((fieldCount - 1) + ", " + a.getDTOAttribute().getSelectTokenConstant() + ", ");
			b.append(i18n.getI18N(a.getDTOAttribute(), a.getLabel()) + ", SearchFieldDataTypeEnum.STRING, 80);\n");
			b.append(searchFieldName + ".setFilterCriteria(filter);\n\n");
			b.append("searchObj.getSearchFields().add(" + searchFieldName + ");\n");
			b.append("}\n\n");

			fieldCount++;
		}

		b.append("buildTree();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator#
	 * addItemMethods(net.codecadenza.eclipse.model.client.TreeViewItem)
	 */
	@Override
	protected void addItemMethods(TreeViewItem treeItem) {
		final DomainObject itemDomainObject = treeItem.getItemDTO().getDomainObject();
		final BoundaryBean itemBoundary = project.getBoundaryByDomainObject(itemDomainObject);
		final String label = itemDomainObject.getLabel();
		final DTOBeanAttribute pkDTOAttribute = treeItem.getItemDTO().getPKAttribute();
		final String pkGetter = pkDTOAttribute.getGetterName();
		var b = new StringBuilder();
		BoundaryMethod deleteMethod = null;

		// We have to check if we delete an object or if we remove an object from an association list!
		final BoundaryMethod removeMethod = getSubItemRemoveMethod(treeItem);

		if (removeMethod != null) {
			final TreeViewItem subTreeItem = treeItem;
			final String refPKGetter = subTreeItem.getItemDTO().getPKAttribute().getGetterName();
			final var parentDTOName = subTreeItem.getAssociation().getUpperCaseName() + "TreeHelperDTO";
			final var methodSignature = "boolean " + removeMethod.getName() + "(" + parentDTOName + " parent, "
					+ treeItem.getItemDTO().getName() + " item)";
			final var declarationGenerator = new ServiceDeclarationGenerator(this, removeMethod, b);
			final String assocName = subTreeItem.getAssociation().getName();

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b.append(removeMethod.generateBeginOfJavadocComment());
				b.append(" * @param parent\n");
				b.append(" * @param item\n");
				b.append(" * @return true if operation was finished successfully!\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(parent == null || item == null)\n");
				b.append("return false;\n\n");

				declarationGenerator.addLocalVariable();

				b.append("\n");
				b.append("try\n");
				b.append("{\n");
				b.append("final boolean delete = MessageDialog.openQuestion(parentShell, ");
				b.append(i18n.getI18NMessage("msg_title_remove", "Remove item") + ", ");
				b.append(i18n.getI18NMessage("msg_conf_remove", "Do you really want to remove selected item from list?") + ");\n\n");
				b.append("if(!delete)\n");
				b.append("return false;\n\n");
				b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");

				final var logMsg = "Remove selected object with id '{}' from list '" + assocName + "' of parent object with id '{}'";

				addDebugLog(b, logMsg, "item." + refPKGetter, "parent.getParentId()");

				b.append("\n");

				new ServiceInvocationGenerator(removeMethod, b).addInvocation("parent.getParentId()", "item." + refPKGetter);

				b.append("return true;\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while removing selected object!", "e");

				b.append("\n");
				b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_remove", "Remove item") + ", ");
				b.append(i18n.getI18NMessage("msg_err_remove", "Remove operation for selected item failed! Message: "));
				b.append(" + e.getMessage());\n");
				b.append("return false;\n");
				b.append("}\n");
				b.append("finally\n");
				b.append("{\n");
				b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_ARROW));\n");

				declarationGenerator.addCloseStatement();

				b.append("}\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (removeMethod == null)
			deleteMethod = getDeleteMethod(treeItem);

		if (deleteMethod != null) {
			final var methodSignature = "boolean delete" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " dto)";

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Delete " + label + "\n");
				b.append(" * @param dto\n");
				b.append(" * @return true if operation was finished successfully!\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(dto == null)\n");
				b.append("return false;\n\n");

				final var declarationGenerator = new ServiceDeclarationGenerator(this, itemBoundary, b);
				declarationGenerator.addLocalVariable();

				b.append("\n");
				b.append("try\n");
				b.append("{\n");
				b.append("final boolean delete = MessageDialog.openQuestion(parentShell, ");
				b.append(i18n.getI18NMessage("msg_title_delete", "Delete object") + ", ");
				b.append(i18n.getI18NMessage("msg_conf_delete", "Do you really want to delete selected object?") + ");\n\n");
				b.append("if(!delete)\n");
				b.append("return false;\n\n");
				b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");

				addDebugLog(b, "Delete selected object with id '{}'", "dto." + pkGetter);

				b.append("\n");

				new ServiceInvocationGenerator(deleteMethod, b).addInvocation("dto." + pkGetter);

				b.append("return true;\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while deleting selected object!", "e");

				b.append("\n");
				b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ", ");
				b.append(i18n.getI18NMessage("msg_err_delete", "Could not delete selected object! Message: ") + " + e.getMessage());\n");
				b.append("return false;\n");
				b.append("}\n");
				b.append("finally\n");
				b.append("{\n");
				b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_ARROW));\n");

				declarationGenerator.addCloseStatement();

				b.append("}\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		// Check if a form of type 'ADD' exists
		Form addForm = null;

		if (treeItem.isRootItem() && tree.getRecursiveMethod() != null)
			addForm = getAddForm(treeItem);

		if (addForm != null) {
			final var methodSignature = "boolean add" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " dto)";

			if (!methodSet.contains(methodSignature)) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
				importClass("org.eclipse.jface.dialogs.Dialog");

				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Add " + label + "\n");
				b.append(" * @param dto\n");
				b.append(" * @return true if operation was finished successfully!\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(dto == null)\n");
				b.append("return false;\n\n");
				b.append("final var dlg = new " + addForm.getName() + "(parentShell, dto." + pkGetter + ");\n");
				b.append("final int result = dlg.open();\n\n");
				b.append("return result == Dialog.OK;\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		// Check if a form of type 'UPDATE' exists
		final Form editForm = getEditForm(treeItem);

		if (editForm != null) {
			final var methodSignature = "boolean edit" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " dto)";

			if (!methodSet.contains(methodSignature)) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
				importClass("org.eclipse.jface.dialogs.Dialog");

				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Edit selected " + label + "\n");
				b.append(" * @param dto\n");
				b.append(" * @return true if operation was finished successfully!\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(dto == null)\n");
				b.append("return false;\n\n");
				b.append("final var dlg = new " + editForm.getName() + "(parentShell, dto." + pkGetter + ");\n");
				b.append("final int result = dlg.open();\n\n");
				b.append("return result == Dialog.OK;\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		// Check if a form of type 'READONLY' exists
		final Form viewForm = getReadOnlyForm(treeItem);

		if (viewForm != null) {
			final var methodSignature = "void view" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " dto)";

			if (!methodSet.contains(methodSignature)) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * View selected " + label + "\n");
				b.append(" * @param dto\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(dto == null)\n");
				b.append("return;\n\n");
				b.append("new " + viewForm.getName() + "(parentShell, dto." + pkGetter + ").open();\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		for (final DomainAttribute attr : itemDomainObject.getAllLobAttributes()) {
			final BoundaryMethod downloadMethod = getDownloadMethod(itemDomainObject, attr);
			var methodName = "";

			if (downloadMethod == null)
				continue;

			if (attr.getDomainObject().equals(itemDomainObject))
				methodName = "download" + attr.getUpperCaseName();
			else
				methodName = "download" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

			final var methodSignature = "void " + methodName + "(final " + treeItem.getItemDTO().getName() + " dto)";

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Download " + attr.getLabel() + "\n");
				b.append(" * @param dto\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("if(dto == null)\n");
				b.append("return;\n\n");
				b.append(new EclipseFileHandlingGenerator(this, downloadMethod, i18n).createDownloadFragment("dto." + pkGetter));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator#
	 * addTreeParentMenuMethods(java.util.Collection)
	 */
	@Override
	protected void addTreeParentMenuMethods(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			final String assocName = item.getAssociation().getUpperCaseName();
			final var dtoName = assocName + "TreeHelperDTO";
			final var menuMethodName = "initialize" + assocName + "Menu";
			final var methodSignature = "void " + menuMethodName + "()";
			final var b = new StringBuilder();

			menuMethodInitializerSet.add(menuMethodName);
			importPackage("org.eclipse.swt.events");

			b.append("/**\n");
			b.append(" * Initialize menu\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");
			b.append("menu" + assocName + " = new Menu(tree);\n\n");
			b.append("// Add menu item to refresh sub-items\n");
			b.append("final var itemRefresh = new MenuItem(menu" + assocName + ", SWT.NONE);\n");
			b.append("itemRefresh.setText(" + i18n.getI18NMessage("action_name_refresh", "Refresh") + ");\n");
			b.append("itemRefresh.setImage(ImageCache.getImage(ImageCache.IMG_REFRESH));\n\n");
			b.append("itemRefresh.addSelectionListener(new SelectionAdapter()\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void widgetSelected(SelectionEvent e)\n");
			b.append("{\n");
			b.append("final TreeItem selItem = getSelectedItem();\n\n");
			b.append("if(selItem == null)\n");
			b.append("return;\n\n");
			b.append("final var dto = (" + dtoName + ") selItem.getData();\n");
			b.append("add" + assocName + "(selItem, dto);\n");
			b.append("}\n");
			b.append("});\n\n");

			// Check if a form of type 'ADD' exists
			if (item.getAssociation() instanceof OneToManyAssociation) {
				final Form addForm = getAddForm(item);

				if (addForm != null) {
					final var fb = new StringBuilder();
					fb.append("// Add menu item to add " + item.getItemDTO().getDomainObject().getLabel() + "\n");
					fb.append("final var itemAdd = new MenuItem(menu" + assocName + ", SWT.NONE);\n");
					fb.append("itemAdd.setText(" + i18n.getI18NMessage("action_name_add", "Add") + ");\n");
					fb.append("itemAdd.setImage(ImageCache.getImage(ImageCache.IMG_NEW_DATA));\n\n");
					fb.append("itemAdd.addSelectionListener(new SelectionAdapter()\n");
					fb.append("{\n");
					fb.append("/* (non-Javadoc)\n");
					fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
					fb.append(" */\n");
					fb.append("@Override\n");
					fb.append("public void widgetSelected(SelectionEvent e)\n");
					fb.append("{\n");
					fb.append("final TreeItem selItem = getSelectedItem();\n\n");
					fb.append("if(selItem == null)\n");
					fb.append("return;\n\n");

					final var methodName = "add" + item.getItemDTO().getDomainObject().getName() + "To" + assocName;

					fb.append("final var dto = (" + dtoName + ") selItem.getData();\n\n");
					fb.append("if(" + methodName + "(dto) && dto.isDataLoaded())\n");
					fb.append("{\n");
					fb.append("for(final TreeItem i : selItem.getItems())\n");
					fb.append("i.dispose();\n\n");
					fb.append("add" + assocName + "(selItem, dto);\n");
					fb.append("selItem.setExpanded(true);\n");
					fb.append("}\n");
					fb.append("}\n");
					fb.append("});\n");

					b.append(securityHelper.wrapSecurityCode(addForm.getRoles(), fb.toString()));
					b.append("\n");
				}
			}

			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		});
	}

	/**
	 * Add tree items
	 * @param parentItemName
	 * @param item
	 * @return the generated content
	 */
	private String addTreeItems(String parentItemName, TreeViewItem item) {
		final var b = new StringBuilder();
		final var itemName = "item" + item.getItemDTO().getDomainObject().getName();

		b.append("itemText.setLength(0);\n");

		boolean isFirstAttribute = true;
		boolean isSecondAttribute = false;

		for (final DTOBeanAttribute attr : item.getDisplayAttributes()) {
			if (attr.getDomainAttribute().getJavaType() instanceof final JavaEnum javaEnum)
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);

			if (isFirstAttribute) {
				isFirstAttribute = false;
				isSecondAttribute = true;
			}
			else if (isSecondAttribute) {
				b.append("itemText.append(\" (\" + " + i18n.getI18N(attr) + " + ITEM_LABEL_SEPARATOR);\n");
				isSecondAttribute = false;
			}
			else
				b.append("itemText.append(\" \" + " + i18n.getI18N(attr) + " + ITEM_LABEL_SEPARATOR);\n");

			final DomainAttribute domainAttribute = attr.getDomainAttribute();
			final String getter = attr.getGetterName();
			final boolean addNullCheck = !attr.getSearchType().isPrimitive();

			if (addNullCheck)
				b.append("\nif(i." + getter + " != null)\n");

			b.append("itemText.append(" + domainAttribute.convertToString("i." + getter) + ");\n");

			if (addNullCheck)
				b.append("\n");
		}

		if (item.getDisplayAttributes().size() > 1)
			b.append("itemText.append(\")\");\n");

		b.append("\n");
		b.append("\nfinal var " + itemName + " = new TreeItem(" + parentItemName + ", SWT.NONE);\n");
		b.append(itemName + ".setText(itemText.toString());\n");
		b.append(itemName + ".setData(i);\n");

		if (item.isRootItem())
			b.append(itemName + ".setImage(ImageCache.getImage(ImageCache.IMG_TREE_FOLDER));\n");
		else
			b.append(itemName + ".setImage(ImageCache.getImage(ImageCache.IMG_TREE_ITEM));\n");

		// Add an item including the helper DTO only for recursive structures
		if (item.isRootItem() && tree.getRecursiveMethod() != null) {
			final String domainObjectName = item.getItemDTO().getDomainObject().getName();
			final var dtoName = domainObjectName + "TreeHelperDTO";
			final var dtoPropertyName = domainObjectName.substring(0, 1).toLowerCase() + domainObjectName.substring(1)
					+ "TreeHelperDTO";
			final String pkGetter = item.getItemDTO().getPKAttribute().getGetterName();

			b.append("\nfinal var " + dtoPropertyName + " = new " + dtoName);
			b.append("(i." + pkGetter + ");\n");
			b.append(dtoPropertyName + ".setDataLoaded(false);\n");
			b.append(itemName + ".setData(ITEM_DATA_KEY, " + dtoPropertyName + ");\n\n");

			if (item.getNodes().isEmpty() && item.getChildren().isEmpty()) {
				b.append("\n// Add dummy item!\n");
				b.append("new TreeItem(" + itemName + ", SWT.NONE);\n\n");
			}
		}

		// Add item nodes
		item.getNodes().forEach(node -> {
			final var nodeName = "item" + node.getDTOAttribute().getUpperCaseName();
			final JavaType type = node.getDTOAttribute().getSearchType();
			final String getter = "i." + node.getDTOAttribute().getGetterName();
			final boolean addNullCheck = !type.isPrimitive();

			if (addNullCheck) {
				b.append("\nif(" + getter + " != null)\n");
				b.append("{");
			}

			if (node.getDTOAttribute().getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE) {
				final var parentNodeName = "parent" + node.getDTOAttribute().getUpperCaseName();

				b.append("\nfinal var " + parentNodeName + " = new TreeItem(" + itemName + ", SWT.NONE);\n");
				b.append(parentNodeName + ".setText(" + i18n.getI18N(node.getDTOAttribute(), node.getLabel()) + ");\n");
				b.append(parentNodeName + ".setImage(ImageCache.getImage(ImageCache.IMG_TREE_ITEMS));\n\n");
				b.append("for(final var element : " + getter + ")\n");
				b.append("{\n");
				b.append("final var " + nodeName + " = new TreeItem(" + parentNodeName + ", SWT.NONE);\n");
				b.append(nodeName + ".setText(" + node.getDTOAttribute().getDomainAttribute().convertToString("element") + ");\n");
				b.append(nodeName + ".setImage(ImageCache.getImage(ImageCache.");

				if (type.isTemporalType())
					b.append("IMG_CALENDAR));\n");
				else
					b.append("IMG_TREE_DATA));\n");

				b.append("}\n");
			}
			else {
				b.append("\nfinal var " + nodeName + " = new TreeItem(" + itemName + ", SWT.NONE);\n");

				if (type.isBoolean()) {
					b.append(nodeName + ".setText(" + i18n.getI18N(node.getDTOAttribute(), node.getLabel()) + ");\n\n");
					b.append("if(" + getter + ")\n");
					b.append(nodeName + ".setImage(ImageCache.getImage(ImageCache.IMG_CHECKED));\n");
					b.append("else\n");
					b.append(nodeName + ".setImage(ImageCache.getImage(ImageCache.IMG_UNCHECKED));\n\n");
				}
				else {
					if (type instanceof final JavaEnum javaEnum)
						javaEnum.getEnumerationValues().forEach(i18n::getI18N);

					b.append(nodeName + ".setText(" + i18n.getI18N(node.getDTOAttribute(), node.getLabel()));
					b.append(" + ITEM_LABEL_SEPARATOR + ");
					b.append(node.getDTOAttribute().getDomainAttribute().convertToString(getter) + ");\n");
					b.append(nodeName + ".setImage(ImageCache.getImage(ImageCache.");

					if (type.isTemporalType())
						b.append("IMG_CALENDAR));\n");
					else
						b.append("IMG_TREE_DATA));\n");
				}
			}

			if (addNullCheck)
				b.append("}\n");
		});

		// Add parent items for all sub-items
		item.getChildren().forEach(treeItem -> {
			final var helperDtoClassName = treeItem.getAssociation().getUpperCaseName() + "TreeHelperDTO";
			final var helperDtoPropertyName = treeItem.getAssociation().getName() + "TreeHelperDTO";

			b.append("\nfinal var " + helperDtoPropertyName + " = new " + helperDtoClassName);
			b.append("(i." + item.getItemDTO().getPKAttribute().getGetterName() + ");\n\n");

			final var subParentItemName = "item" + treeItem.getAssociation().getUpperCaseName();
			final var msgKey = tree.getName() + "_" + treeItem.getAssociation().getName();

			b.append("\nfinal var " + subParentItemName + " = new TreeItem(" + itemName + ", SWT.NONE);\n");
			b.append(subParentItemName + ".setText(" + i18n.getI18NMessage(msgKey, treeItem.getLabel()) + ");\n");
			b.append(subParentItemName + ".setData(" + helperDtoPropertyName + ");\n");
			b.append(subParentItemName + ".setImage(ImageCache.getImage(ImageCache.IMG_TREE_ITEMS));\n\n");
			b.append("// Add dummy item!\n");
			b.append("new TreeItem(" + subParentItemName + ", SWT.NONE);\n\n");
		});

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator#addItemFetchMethods(java.util.Collection)
	 */
	@Override
	protected void addItemFetchMethods(Collection<TreeViewItem> items) {
		items.forEach(treeItem -> {
			final String assocName = treeItem.getAssociation().getUpperCaseName();
			final var dtoName = assocName + "TreeHelperDTO";
			final BoundaryMethod m = treeItem.getDataFetchMethod();
			var methodSignature = "void add" + assocName + "(TreeItem parentItem, " + dtoName + " dto)";
			var b = new StringBuilder();

			// Add the method to fetch the sub-items
			b.append("/**\n");
			b.append(" * Add " + treeItem.getItemDTO().getDomainObject().getLabel() + " items to parent\n");
			b.append(" * @param parentItem\n");
			b.append(" * @param dto\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");

			addDebugLog(b, "Perform data fetch operation for " + treeItem.getItemDTO().getDomainObject().getLabel() + " sub-items");

			b.append("\n");

			final var declarationGenerator = new ServiceDeclarationGenerator(this, m, b);
			declarationGenerator.addLocalVariable();

			b.append("\n");
			b.append("try\n");
			b.append("{\n");
			b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");
			b.append("parentItem.removeAll();\n\n");
			b.append("final java.util.List<" + treeItem.getItemDTO().getName() + "> items = ");

			new ServiceInvocationGenerator(m, b).addInvocation("dto.getParentId()");

			final var compName = assocName + "Comparator";

			b.append("Collections.sort(items, new " + compName + "());\n");
			b.append("final var itemText = new StringBuilder();\n\n");
			b.append("for(final " + treeItem.getItemDTO().getName() + " i : items)\n");
			b.append("{\n");
			b.append(addTreeItems("parentItem", treeItem));
			b.append("}\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while fetching data!", "e");

			b.append("\n");
			b.append("lblStateImage.setImage(ImageCache.getImage(ImageCache.IMG_ERROR));\n");
			b.append("lblResult.setText(" + i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
			b.append(" + e.getMessage());\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_ARROW));\n");

			declarationGenerator.addCloseStatement();

			b.append("}\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());

			// Check if a form of type 'ADD' exists
			if (treeItem.getAssociation() instanceof OneToManyAssociation) {
				final TreeViewItem parentItem = treeItem.getParentItem();
				final Form addForm = getAddForm(treeItem);

				if (addForm != null) {
					final var methodName = "add" + treeItem.getItemDTO().getDomainObject().getName() + "To" + assocName;
					methodSignature = "boolean " + methodName + "(" + dtoName + " dto)";

					importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
					importClass("org.eclipse.jface.dialogs.Dialog");

					b = new StringBuilder();
					b.append("/**\n");
					b.append(" * Add " + treeItem.getItemDTO().getDomainObject().getLabel() + " to ");
					b.append(parentItem.getItemDTO().getDomainObject().getLabel() + "\n");
					b.append(" * @param dto\n");
					b.append(" * @return true if operation was finished successfully!\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private " + methodSignature + "\n");
					b.append("{\n");
					b.append("final var dlg = new " + addForm.getName() + "(parentShell, dto.getParentId());\n");
					b.append("final int result = dlg.open();\n\n");
					b.append("return result == Dialog.OK;\n");
					b.append("}\n\n");

					addMethod(methodSignature, b.toString());
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator#
	 * addItemMenuMethod(net.codecadenza.eclipse.model.client.TreeViewItem)
	 */
	@Override
	protected void addItemMenuMethod(TreeViewItem treeItem) {
		final var b = new StringBuilder();
		final DomainObject itemDomainObject = treeItem.getItemDTO().getDomainObject();
		final String label = itemDomainObject.getLabel();
		final var menuMethodName = "initialize" + itemDomainObject.getName() + "Menu";
		final var methodSignature = "void " + menuMethodName + "()";

		menuMethodInitializerSet.add(menuMethodName);

		if (methodSet.contains(methodSignature))
			return;

		methodSet.add(methodSignature);

		b.append("/**\n");
		b.append(" * Initialize " + label + " menu\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("menu" + itemDomainObject.getName() + " = new Menu(tree);\n\n");

		// Check if form of type 'ADD' exists
		Form addForm = null;
		String fetchMethodName = null;

		if (treeItem.isRootItem()) {
			fetchMethodName = "add" + treeItem.getItemDTO().getDomainObject().getNamePlural() + "OfParent"
					+ treeItem.getItemDTO().getDomainObject().getName();

			if (tree.getRecursiveMethod() != null)
				addForm = getAddForm(treeItem);
		}

		if (addForm != null) {
			importPackage("org.eclipse.swt.events");

			final var fb = new StringBuilder();
			fb.append("// Add menu item to add a new item\n");
			fb.append("final var itemAdd = new MenuItem(menu" + itemDomainObject.getName() + ", SWT.NONE);\n");
			fb.append("itemAdd.setText(" + i18n.getI18NMessage("action_name_add", "Add") + ");\n");
			fb.append("itemAdd.setImage(ImageCache.getImage(ImageCache.IMG_NEW_DATA));\n\n");
			fb.append("itemAdd.addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");
			fb.append("final TreeItem selItem = getSelectedItem();\n\n");
			fb.append("if(selItem == null)\n");
			fb.append("return;\n\n");
			fb.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getData();\n\n");
			fb.append("if(add" + itemDomainObject.getName() + "(dto))\n");
			fb.append("{\n");
			fb.append("final var parent = (" + itemDomainObject.getName() + "TreeHelperDTO) selItem.getData(ITEM_DATA_KEY);\n\n");
			fb.append("if(!parent.isDataLoaded())\n");
			fb.append("return;\n\n");
			fb.append("for(final TreeItem i : selItem.getItems())\n");
			fb.append("if(i.getData() instanceof " + treeItem.getItemDTO().getName() + ")\n");
			fb.append("i.dispose();\n\n");
			fb.append(fetchMethodName + "(selItem, parent);\n");
			fb.append("parent.setDataLoaded(true);\n");
			fb.append("selItem.setExpanded(true);\n");
			fb.append("}\n}\n");
			fb.append("});\n");

			b.append(securityHelper.wrapSecurityCode(addForm.getRoles(), fb.toString()));
			b.append("\n");
		}

		// Check if a form of type 'UPDATE' exists
		final Form editForm = getEditForm(treeItem);

		if (editForm != null) {
			importPackage("org.eclipse.swt.events");

			final var fb = new StringBuilder();
			fb.append("// Add menu item to edit item\n");
			fb.append("final var itemEdit = new MenuItem(menu" + itemDomainObject.getName() + ", SWT.NONE);\n");
			fb.append("itemEdit.setText(" + i18n.getI18NMessage("action_name_edit", "Edit") + ");\n");
			fb.append("itemEdit.setImage(ImageCache.getImage(ImageCache.IMG_EDIT_DATA));\n\n");
			fb.append("itemEdit.addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");
			fb.append("final TreeItem selItem = getSelectedItem();\n\n");
			fb.append("if(selItem == null)\n");
			fb.append("return;\n\n");
			fb.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getData();\n");
			fb.append("edit" + itemDomainObject.getName() + "(dto);\n");
			fb.append("}\n");
			fb.append("});\n");

			b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), fb.toString()));
			b.append("\n");
		}

		// Check if a form of type 'READONLY' exists
		final Form viewForm = getReadOnlyForm(treeItem);

		if (viewForm != null) {
			importPackage("org.eclipse.swt.events");

			final var fb = new StringBuilder();
			fb.append("// Add menu item to view item\n");
			fb.append("final var itemView = new MenuItem(menu" + itemDomainObject.getName() + ", SWT.NONE);\n");
			fb.append("itemView.setText(" + i18n.getI18NMessage("action_name_view", "View") + ");\n");
			fb.append("itemView.setImage(ImageCache.getImage(ImageCache.IMG_VIEW_DATA));\n\n");
			fb.append("itemView.addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");
			fb.append("final TreeItem selItem = getSelectedItem();\n\n");
			fb.append("if(selItem == null)\n");
			fb.append("return;\n\n");
			fb.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getData();\n");
			fb.append("view" + itemDomainObject.getName() + "(dto);\n");
			fb.append("}\n");
			fb.append("});\n");

			b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), fb.toString()));
			b.append("\n");
		}

		// Add the context menu items for performing download operations
		for (final DomainAttribute attr : itemDomainObject.getAllLobAttributes()) {
			final BoundaryMethod downloadMethod = getDownloadMethod(itemDomainObject, attr);
			var methodName = "";

			importPackage("org.eclipse.swt.events");

			if (downloadMethod == null)
				continue;

			if (attr.getDomainObject().equals(itemDomainObject))
				methodName = "download" + attr.getUpperCaseName();
			else
				methodName = "download" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

			final var menuItemName = methodName + "Item";

			final var fb = new StringBuilder();
			fb.append("// Add menu item to download " + attr.getLabel() + "\n");
			fb.append("final var " + menuItemName + " = new MenuItem(menu" + itemDomainObject.getName() + ", SWT.NONE);\n");
			fb.append(menuItemName + ".setText(" + i18n.getI18NMessage("action_name_download", "Download") + ");\n");
			fb.append(menuItemName + ".setImage(ImageCache.getImage(\"download.png\"));\n\n");
			fb.append(menuItemName + ".addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");
			fb.append("final TreeItem selItem = getSelectedItem();\n\n");
			fb.append("if(selItem == null)\n");
			fb.append("return;\n\n");
			fb.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getData();\n");
			fb.append(methodName + "(dto);\n");
			fb.append("}\n");
			fb.append("});\n");

			if (addSecurity && downloadMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(downloadMethod.getRoles(), fb.toString()));
			else
				b.append(fb.toString());

			b.append("\n");
		}

		// Check if either a delete or remove method should be added
		BoundaryMethod deleteMethod = null;
		BoundaryMethod removeMethod = null;

		if (treeItem.isRootItem() || treeItem.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
			deleteMethod = getDeleteMethod(treeItem);
		else
			removeMethod = getSubItemRemoveMethod(treeItem);

		if (deleteMethod != null) {
			importPackage("org.eclipse.swt.events");

			final var fb = new StringBuilder();
			fb.append("// Add menu item to remove item\n");
			fb.append("final var itemRemove = new MenuItem(menu" + itemDomainObject.getName() + ", SWT.NONE);\n");
			fb.append("itemRemove.setText(" + i18n.getI18NMessage("action_name_delete", "Delete") + ");\n");
			fb.append("itemRemove.setImage(ImageCache.getImage(ImageCache.IMG_DELETE));\n\n");
			fb.append("itemRemove.addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");
			fb.append("final TreeItem selItem = getSelectedItem();\n\n");
			fb.append("if(selItem == null)\n");
			fb.append("return;\n\n");
			fb.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getData();\n");
			fb.append("final boolean success = delete" + itemDomainObject.getName() + "(dto);\n\n");
			fb.append("if(success)\n");
			fb.append("selItem.dispose();\n");
			fb.append("}\n");
			fb.append("});\n");

			if (addSecurity && deleteMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(deleteMethod.getRoles(), fb.toString()));
			else
				b.append(fb.toString());

			b.append("\n");
		}

		if (removeMethod != null) {
			final TreeViewItem subTreeItem = treeItem;
			final var parentDTOName = subTreeItem.getAssociation().getUpperCaseName() + "TreeHelperDTO";
			final String dtoName = subTreeItem.getItemDTO().getName();
			final var fb = new StringBuilder();

			importPackage("org.eclipse.swt.events");

			fb.append("// Add menu item to remove item from collection\n");
			fb.append("final var itemRemove = new MenuItem(menu" + itemDomainObject.getName() + ", SWT.NONE);\n");
			fb.append("itemRemove.setText(" + i18n.getI18NMessage("action_name_remove", "Remove") + ");\n");
			fb.append("itemRemove.setImage(ImageCache.getImage(ImageCache.IMG_DELETE));\n\n");
			fb.append("itemRemove.addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");
			fb.append("final TreeItem selItem = getSelectedItem();\n\n");
			fb.append("if(selItem == null)\n");
			fb.append("return;\n\n");
			fb.append("final var item = (" + dtoName + ") selItem.getData();\n");
			fb.append("final var parent = (" + parentDTOName + ") selItem.getParentItem().getData();\n");
			fb.append("final boolean success = " + removeMethod.getName() + "(parent, item);\n\n");
			fb.append("if(success)\n");
			fb.append("selItem.dispose();\n");
			fb.append("}\n");
			fb.append("});\n");

			if (addSecurity && removeMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(removeMethod.getRoles(), fb.toString()));
			else
				b.append(fb.toString());

			b.append("\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the internal class that is responsible for fetching data
	 */
	private void addDataFetchJob() {
		final var b = new StringBuilder();
		final var className = "DataFetchJob";

		b.append("/**\n");
		b.append(" * Job that is responsible for data fetch operation\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected class " + className + " extends Job\n");
		b.append("{\n");
		b.append("private boolean ready;\n");
		b.append("private boolean error;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public " + className + "()\n");
		b.append("{\n");
		b.append("super(" + i18n.getI18NMessage("job_name_data_fetch", "Data fetch operation") + ");\n\n");
		b.append("setSystem(true);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return true if the job is ready\n");
		b.append(" */\n");
		b.append("public boolean isReady()\n");
		b.append("{\n");
		b.append("return ready;\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @return true if data fetch operation failed\n");
		b.append(" */\n");
		b.append("public boolean isError()\n");
		b.append("{\n");
		b.append("return error;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected IStatus run(IProgressMonitor monitor)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + rootItem.getItemDTO().getDomainObject().getLabel() + " items");

		b.append("\n");
		b.append("data = ");

		final var invocationGenerator = new ServiceInvocationGenerator(tree.getBoundaryMethod(), b);

		if (tree.needsSearchObject())
			invocationGenerator.addInvocation("searchObj");
		else
			invocationGenerator.addInvocation();

		b.append("\n");
		b.append("if(monitor.isCanceled())\n");
		b.append("{\n");
		b.append("ready = true;\n");
		b.append("return Status.CANCEL_STATUS;\n");
		b.append("}\n\n");

		final var compName = rootItem.getItemDTO().getDomainObject().getName() + "Comparator";

		b.append("Collections.sort(data, new " + compName + "());\n\n");
		b.append("if(monitor.isCanceled())\n");
		b.append("{\n");
		b.append("ready = true;\n");
		b.append("return Status.CANCEL_STATUS;\n");
		b.append("}\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while fetching data!", "e");

		b.append("\n");
		b.append("display.syncExec(() ->\n");
		b.append("{\n");

		if (addAdvSearch)
			b.append("actionAdvSearch.setEnabled(true);\n");

		b.append("actionRefresh.setEnabled(true);\n");
		b.append("actionStopSearch.setEnabled(false);\n\n");

		// Enable all quick-search fields
		tree.getQuickSearchItems().forEach(item -> {
			final var txtName = "txt" + item.getDTOAttribute().getUpperCaseName();

			b.append("if(!" + txtName + ".isDisposed())\n");
			b.append(txtName + ".setEnabled(true);\n\n");
		});

		b.append("if(!lblStateImage.isDisposed())\n");
		b.append("lblStateImage.setImage(ImageCache.getImage(ImageCache.IMG_ERROR));\n\n");
		b.append("if(!lblResult.isDisposed())\n");
		b.append("lblResult.setText(");
		b.append(i18n.getI18NMessage("msg_title_err_fetching_data", "Data fetch operation failed!") + ");\n\n");
		b.append("MessageDialog.openError(parentShell, ");
		b.append(i18n.getI18NMessage("msg_title_err_fetching_data", "Data fetch operation failed!"));
		b.append(", " + i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
		b.append(" + e.getMessage());\n");
		b.append("error = true;\n");
		b.append("});\n");
		b.append("}\n\n");

		addDebugLog(b, "Data fetch operation finished");

		b.append("\n");
		b.append("ready = true;\n");
		b.append("return Status.OK_STATUS;\n");
		b.append("}\n");
		b.append("}\n\n");

		addSubClass(className, b.toString());
	}

	/**
	 * Add the method to build the tree view
	 */
	private void addTreeBuildMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "void buildTree()";

		b.append("/**\n");
		b.append(" * Method to build and refresh tree view content\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var startTime = new GregorianCalendar();\n");
		b.append("actionStopSearch.setEnabled(true);\n");

		if (addAdvSearch)
			b.append("actionAdvSearch.setEnabled(false);\n");

		b.append("actionRefresh.setEnabled(false);\n");
		b.append("lblStateImage.setImage(ImageCache.getImage(ImageCache.IMG_PERFORM_FETCH));\n");
		b.append("lblResult.setText(" + i18n.getI18NMessage("msg_tree_data_fetch_in_progress", "Fetching data...") + ");\n\n");

		// Disable all quick-search fields
		tree.getQuickSearchItems().forEach(item -> {
			final var txtName = "txt" + item.getDTOAttribute().getUpperCaseName();

			b.append(txtName + ".setEnabled(false);\n");
		});

		b.append("\ntree.removeAll();\n\n");
		b.append("queryJob = new Job(" + i18n.getI18NMessage("job_name_query", "Fetching data...") + ")\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.lang.Runnable#run()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public IStatus run(IProgressMonitor monitor)\n");
		b.append("{\n");
		b.append("final var dataFetchJob = new DataFetchJob();\n");
		b.append("dataFetchJob.schedule();\n\n");
		b.append("while(true)\n");
		b.append("{\n");
		b.append("// We wait some time until we check both jobs again!\n");
		b.append("try\n");
		b.append("{\n");
		b.append("Thread.sleep(25);\n");
		b.append("}\n");
		b.append("catch (final InterruptedException e)\n");
		b.append("{\n");
		b.append("Thread.currentThread().interrupt();\n");

		addWarningLog(b, "Data fetch thread has been interrupted!", "e");

		b.append("}\n\n");
		b.append("if(monitor.isCanceled())\n");
		b.append("{\n");
		b.append("// If the main job should be canceled the data fetch job should be canceled too!\n");
		b.append("dataFetchJob.cancel();\n\n");
		b.append("display.syncExec(() ->\n");
		b.append("{\n");

		if (addAdvSearch)
			b.append("actionAdvSearch.setEnabled(true);\n");

		b.append("actionRefresh.setEnabled(true);\n");
		b.append("actionStopSearch.setEnabled(false);\n");

		// Enable all quick-search fields
		tree.getQuickSearchItems().forEach(item -> {
			final var txtName = "txt" + item.getDTOAttribute().getUpperCaseName();

			b.append("\nif(!" + txtName + ".isDisposed())\n");
			b.append(txtName + ".setEnabled(true);\n");
		});

		b.append("\nif(!lblStateImage.isDisposed())\n");
		b.append("lblStateImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));\n\n");
		b.append("if(!lblResult.isDisposed())\n");
		b.append("lblResult.setText(");
		b.append(i18n.getI18NMessage("msg_tree_data_fetch_canceled", "Data fetch operation canceled by user!"));
		b.append(");\n");
		b.append("});\n\n");
		b.append("// The job should be canceled!\n");
		b.append("return Status.CANCEL_STATUS;\n");
		b.append("}\n\n");
		b.append("// The loop will be left when the data fetch job is ready!\n");
		b.append("if(dataFetchJob.isReady())\n");
		b.append("break;\n");
		b.append("}\n\n");
		b.append("// If the data fetch job failed the main job should be stopped as it is in the responsibility ");
		b.append("of the data fetch job to provide an error message to the user!\n");
		b.append("if(dataFetchJob.isError())\n");
		b.append("return Status.CANCEL_STATUS;\n\n");
		b.append("final var counter = new int[1];\n");
		b.append("counter[0] = 0;\n");
		b.append("final var itemText = new StringBuilder();\n\n");
		b.append("display.syncExec(() ->\n");
		b.append("{\n");
		b.append("for(final " + rootItem.getItemDTO().getName() + " i : data)\n");
		b.append("{\n");
		b.append("if(tree.isDisposed())\n");
		b.append("return;\n\n");
		b.append(addTreeItems("tree", rootItem));
		b.append("\n");
		b.append("counter[0] = counter[0] + 1;\n");
		b.append("}\n\n");
		b.append("actionStopSearch.setEnabled(false);\n");

		if (addAdvSearch)
			b.append("actionAdvSearch.setEnabled(true);\n");

		b.append("actionRefresh.setEnabled(true);\n");

		// Enable all quick-search fields
		tree.getQuickSearchItems().forEach(item -> {
			final var txtName = "txt" + item.getDTOAttribute().getUpperCaseName();

			b.append("\nif(!" + txtName + ".isDisposed())\n");
			b.append(txtName + ".setEnabled(true);\n");
		});

		final var messageParameters = "counter[0], decimalFormat.format((System.currentTimeMillis() - startTime.getTimeInMillis()) / 1000.0)";

		b.append("\nif(!lblStateImage.isDisposed())\n");
		b.append("lblStateImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));\n\n");
		b.append("if(!lblResult.isDisposed())\n");
		b.append("lblResult.setText(");
		b.append(i18n.getI18NMessage("msg_tree_data_fetch_finished", "{0} item(s) fetched in {1} seconds!", messageParameters));
		b.append(");\n");
		b.append("});\n\n");
		b.append("return Status.OK_STATUS;\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("queryJob.schedule();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method to fetch the next item hierarchy level
	 * @param rootItem
	 */
	private void addRecursiveMethod(TreeViewItem rootItem) {
		final var b = new StringBuilder();
		final var paramDTOName = rootItem.getItemDTO().getDomainObject().getName() + "TreeHelperDTO";
		final String dtoName = rootItem.getItemDTO().getName();
		final var fetchMethodName = "add" + rootItem.getItemDTO().getDomainObject().getNamePlural() + "OfParent"
				+ rootItem.getItemDTO().getDomainObject().getName();
		final var methodSignature = "void " + fetchMethodName + "(TreeItem parentItem, " + paramDTOName + " dto)";

		b.append("/**\n");
		b.append(" * @param parentItem\n");
		b.append(" * @param dto\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + rootItem.getItemDTO().getDomainObject().getLabel() + " sub-items");

		b.append("\n");
		b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");

		if (rootItem.getNodes().isEmpty() && rootItem.getChildren().isEmpty())
			b.append("parentItem.removeAll();\n\n");

		b.append("try\n");
		b.append("{\n");
		b.append("final java.util.List<" + dtoName + "> items = ");

		new ServiceInvocationGenerator(tree.getRecursiveMethod(), b).addInvocation("dto.getParentId()");

		final var compName = rootItem.getItemDTO().getDomainObject().getName() + "Comparator";

		b.append("Collections.sort(items, new " + compName + "());\n");
		b.append("final var itemText = new StringBuilder();\n\n");
		b.append("for(final " + dtoName + " i : items)\n");
		b.append("{\n");
		b.append(addTreeItems("parentItem", rootItem));
		b.append("}\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while fetching data!", "e");

		b.append("\n");
		b.append("lblStateImage.setImage(ImageCache.getImage(ImageCache.IMG_ERROR));\n");
		b.append("lblResult.setText(" + i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
		b.append(" + e.getMessage());\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_ARROW));\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the method createPartControl()
	 */
	private void addPartControlMethod() {
		final var b = new StringBuilder();
		final BoundaryBean boundaryBean = tree.getBoundaryMethod().getBoundaryBean();
		final var methodSignature = "void createPartControl(Composite parent, Shell parentShell)";
		final String clientFieldInit = new EclipseClientFieldHelper(tree, this).initClientField();

		b.append("/**\n");
		b.append(" * Initialize tree view\n");
		b.append(" * @param parent\n");
		b.append(" * @param parentShell\n");
		b.append(" */\n");
		b.append("@PostConstruct\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Initialize tree view");

		b.append("\n");

		if (!clientFieldInit.isEmpty())
			b.append("this." + clientFieldInit);

		b.append("this.parentShell = parentShell;\n");
		b.append("this.display = parentShell.getDisplay();\n");

		if (addAdvSearch)
			b.append("thisView = this;\n\n");
		else
			b.append("\n");

		b.append("parent.setLayout(new GridLayout(2, false));\n\n");
		b.append("toolBar = new ToolBar(parent, SWT.FLAT | SWT.RIGHT);\n");
		b.append("toolBar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));\n\n");

		new ServiceDeclarationGenerator(this, boundaryBean, b).initField();

		b.append("\n");

		if (addQuickSearch) {
			boolean isFirstItem = true;

			importPackage("org.eclipse.swt.events");

			for (final TreeSearchItem s : tree.getQuickSearchItems()) {
				final var labelName = "lbl" + s.getDTOAttribute().getUpperCaseName();
				final var txtName = "txt" + s.getDTOAttribute().getUpperCaseName();

				b.append("final var " + labelName + " = new Label(parent, SWT.NONE);\n");
				b.append(labelName + ".setText(" + i18n.getI18N(s.getDTOAttribute(), s.getLabel(), true) + ");\n\n");
				b.append(txtName + " = new Text(parent, SWT.BORDER);\n");
				b.append(txtName + ".setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));\n");

				if (isFirstItem) {
					isFirstItem = false;
					b.append(txtName + ".setFocus();\n");
				}

				b.append("\n");
				b.append(txtName + ".addKeyListener(new KeyAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void keyReleased(final KeyEvent e)\n");
				b.append("{\n");
				b.append("if(e.character == SWT.CR)\n");
				b.append("performQuickSearch();\n");
				b.append("}\n");
				b.append("});\n\n");
			}
		}

		if (tree.needsSearchObject()) {
			b.append("// Initialize search object\n");
			b.append("searchObj = new SearchDTO();\n");
			b.append("searchObj.setDateFormat(format.getDateFormat());\n");
			b.append("searchObj.setDateTimeFormat(format.getDateTimeFormat());\n");
			b.append("searchObj.setNumberFormat(format.getDecimalFormat());\n");
			b.append("searchObj.setCaseSensitive(false);\n");
			b.append("searchObj.setExactFilterMatch(true);\n");
			b.append("searchObj.setCount(false);\n");
			b.append("searchObj.setMaxResult(1000);\n\n");
		}

		b.append("tree = new Tree(parent, SWT.BORDER);\n");
		b.append("tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));\n\n");
		b.append("final var panState = new Composite(parent, SWT.BORDER);\n");
		b.append("panState.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));\n\n");
		b.append("final var gridLayout1 = new GridLayout();\n");
		b.append("gridLayout1.marginHeight = 2;\n");
		b.append("gridLayout1.marginWidth = 0;\n");
		b.append("gridLayout1.numColumns = 2;\n\n");
		b.append("panState.setLayout(gridLayout1);\n\n");
		b.append("lblStateImage = new Label(panState, SWT.NONE);\n");
		b.append("lblStateImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));\n\n");
		b.append("lblResult = new Label(panState, SWT.NONE);\n");
		b.append("lblResult.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));\n\n");

		if (addKeyListener())
			b.append("addKeyListener();\n");

		b.append("addMouseAndMenuListener();\n");

		if (!helperDTOMap.isEmpty() || tree.getRecursiveMethod() != null)
			b.append("addTreeListener();\n");

		b.append("\n");
		b.append("createActions();\n\n");
		b.append("initializeToolBar();\n");

		if (!dropItems.isEmpty()) {
			b.append("initializeDragSource();\n");
			b.append("initializeDropTarget();\n");
		}

		// Invoke the context menu initializer methods
		menuMethodInitializerSet.forEach(initMethod -> b.append(initMethod + "();\n"));

		if (!addAdvSearch && !addQuickSearch) {
			b.append("\n");
			b.append("buildTree();\n");
		}

		b.append("\n");

		addDebugLog(b, "Tree view initialization finished");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Method to create actions and action initializer
	 */
	private void addActions() {
		final DomainObject rootDomainObject = rootItem.getItemDTO().getDomainObject();
		var className = "RefreshAction";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Create refresh action\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private class " + className + " extends Action\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public " + className + "()\n");
		b.append("{\n");
		b.append("super(\"\", Action.AS_PUSH_BUTTON);\n\n");
		b.append("this.setToolTipText(" + i18n.getI18NMessage("action_name_refresh", "Refresh") + ");\n");
		b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_REFRESH));\n");
		b.append("}\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.jface.action.Action#run()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void run()\n");
		b.append("{\n");
		b.append("buildTree();\n");
		b.append("}\n");
		b.append("}\n\n");

		addSubClass(className, b.toString());

		if (addAdvSearch) {
			className = "AdvancedSearchAction";
			importClass("org.eclipse.jface.dialogs.Dialog");

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Action to perform advanced search\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private class " + className + " extends Action\n");
			b.append("{\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append("public " + className + "()\n");
			b.append("{\n");
			b.append("super(\"\", Action.AS_PUSH_BUTTON);\n\n");
			b.append("this.setToolTipText(" + i18n.getI18NMessage("action_name_adv_search", "Advanced search") + ");\n");
			b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_SEARCH));\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.jface.action.Action#run()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void run()\n");
			b.append("{\n");
			b.append("SearchDTO searchObjAdv;\n\n");
			b.append("if(SearchManager.getLastSearch(ID) == null)\n");
			b.append("{\n");
			b.append("// Initialize search object\n");
			b.append("searchObjAdv = new SearchDTO();\n");
			b.append("searchObjAdv.setDateFormat(format.getDateFormat());\n");
			b.append("searchObjAdv.setDateTimeFormat(format.getDateTimeFormat());\n");
			b.append("searchObjAdv.setNumberFormat(format.getDecimalFormat());\n");
			b.append("searchObjAdv.setCaseSensitive(false);\n");
			b.append("searchObjAdv.setExactFilterMatch(true);\n");
			b.append("searchObjAdv.setCount(false);\n");
			b.append("searchObjAdv.setMaxResult(1000);\n");

			int fieldIndex = 1;

			for (final TreeSearchItem a : tree.getAdvancedSearchItems()) {
				final var searchFieldName = "field" + fieldIndex;
				final DomainAttribute attribute = a.getDTOAttribute().getDomainAttribute();
				final String fieldType = attribute.getSearchFieldDataType();

				b.append("\n");
				b.append("final var " + searchFieldName + " = new SearchFieldDTO(" + fieldIndex + ", ");
				b.append(a.getDTOAttribute().getSelectTokenConstant() + ", ");
				b.append(i18n.getI18N(a.getDTOAttribute(), a.getLabel()) + ", " + fieldType + ", 80);\n");

				if (attribute.getJavaType().isLocalDate() || attribute.getTemporalType() == TemporalTypeEnumeration.DATE)
					b.append(searchFieldName + ".setDateTimeFormat(false);\n\n");

				b.append("searchObjAdv.getSearchFields().add(" + searchFieldName + ");\n");

				if (attribute.getJavaType().isEnum()) {
					final var javaEnum = (JavaEnum) attribute.getJavaType();

					// Generate translations for all literals
					javaEnum.getEnumerationValues().forEach(i18n::getI18N);

					b.append("\nfinal var enumListValues" + fieldIndex + " = new HashMap<String, String>();\n");
					b.append("enumListValues" + fieldIndex + ".put(\"\", \"\");\n");

					for (final EnumLiteral value : javaEnum.getEnumerationValues()) {
						b.append("enumListValues" + fieldIndex + ".put(\"" + value.getName() + "\", getTranslation");
						b.append("(" + javaEnum.getName().toUpperCase() + "_" + value.getName().toUpperCase() + "));\n");
					}

					b.append("\n" + searchFieldName + ".setEnumListValues(enumListValues" + fieldIndex + ");\n");
				}

				fieldIndex++;
			}

			b.append("}\n");
			b.append("else\n");
			b.append("searchObjAdv = SearchManager.getLastSearch(ID);\n\n");
			b.append("final var dlg = new SearchInputDialog(parentShell, searchObjAdv, thisView);\n\n");
			b.append("if(Dialog.OK == dlg.open())\n");
			b.append("{\n");
			b.append("searchObjAdv = dlg.getSearchInput();\n");
			b.append("searchObj = searchObjAdv;\n\n");
			b.append("SearchManager.saveLastSearch(ID, searchObjAdv);\n");
			b.append("buildTree();\n");
			b.append("}\n");
			b.append("}\n");
			b.append("}\n\n");

			addSubClass(className, b.toString());
		}

		// Add the form action for creating a new object
		final Form createForm = getCreateNewForm(rootItem);

		if (createForm != null) {
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
			importClass("org.eclipse.jface.dialogs.Dialog");

			className = "Create" + rootDomainObject.getName() + "Action";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Action to create new " + rootDomainObject.getLabel() + "\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private class " + className + " extends Action\n");
			b.append("{\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append("public " + className + "()\n");
			b.append("{\n");
			b.append("super(\"\", Action.AS_PUSH_BUTTON);\n\n");
			b.append("this.setToolTipText(" + i18n.getI18NMessage("action_name_create", "Create") + ");\n");
			b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_NEW_DATA));\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.jface.action.Action#run()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void run()\n");
			b.append("{\n");
			b.append("final var dlg = new " + createForm.getName() + "(parentShell);\n");
			b.append("final int returnValue = dlg.open();\n\n");
			b.append("if(returnValue == Dialog.OK)\n");
			b.append("buildTree();\n");
			b.append("}\n");
			b.append("}\n\n");

			addSubClass(className, b.toString());
		}

		className = "StopSearchAction";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Action to stop search thread\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private class " + className + " extends Action\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public " + className + "()\n");
		b.append("{\n");
		b.append("super(\"\", Action.AS_PUSH_BUTTON);\n\n");
		b.append("this.setToolTipText(" + i18n.getI18NMessage("action_name_stop_search", "Stop search") + ");\n");
		b.append("this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_STOP_PROCESS));\n");
		b.append("this.setEnabled(false);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.jface.action.Action#run()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void run()\n");
		b.append("{\n");
		b.append("if(queryJob != null)\n");
		b.append("queryJob.cancel();\n");
		b.append("}\n");
		b.append("}\n\n");

		addSubClass(className, b.toString());

		var methodSignature = "void createActions()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Create the actions\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("actionRefresh = new RefreshAction();\n");

		if (addAdvSearch)
			b.append("actionAdvSearch = new AdvancedSearchAction();\n");

		if (createForm != null) {
			final var actionName = "actionCreate" + rootDomainObject.getName();
			final var actionClassName = "Create" + rootDomainObject.getName() + "Action";

			b.append(actionName + " = new " + actionClassName + "();\n");
		}

		b.append("actionStopSearch = new StopSearchAction();\n");
		b.append("actionStopSearch.setEnabled(false);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void initializeToolBar()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize the toolbar\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("toolBarManager = new ToolBarManager(toolBar);\n");
		b.append("toolBarManager.add(actionRefresh);\n");

		if (addAdvSearch)
			b.append("toolBarManager.add(actionAdvSearch);\n");

		b.append("toolBarManager.add(actionStopSearch);\n");

		if (createForm != null) {
			final var actionName = "actionCreate" + rootDomainObject.getName();

			b.append("\n");
			b.append(securityHelper.wrapSecurityCode(createForm.getRoles(), "toolBarManager.add(" + actionName + ");\n"));
		}

		b.append("\n");
		b.append("toolBarManager.update(true);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the drag source listener method
	 */
	private void createDragSourceListener() {
		final var b = new StringBuilder();
		final var methodSignature = "void initializeDragSource()";
		boolean isFirstDTO = true;

		if (dropItems.isEmpty())
			return;

		b.append("/**\n");
		b.append(" * Initialize tree to support drag operations\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var dragSource = new DragSource(tree, DND.DROP_MOVE);\n");

		if (project.getClientPlatform() == ClientPlatformEnumeration.RCP)
			b.append("dragSource.setTransfer(TextTransfer.getInstance());\n\n");
		else
			b.append("dragSource.setTransfer(new Transfer[] { TextTransfer.getInstance() });\n\n");

		b.append("dragSource.addDragListener(new DragSourceAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.dnd.DragSourceAdapter#dragStart(org.eclipse.swt.dnd.DragSourceEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void dragStart(DragSourceEvent event)\n");
		b.append("{\n");
		b.append("final TreeItem selItem = getSelectedItem();\n");
		b.append("dragSourceItem = null;\n\n");
		b.append("if(selItem == null || selItem.getData() == null)\n");
		b.append("{\n");
		b.append("event.doit = false;\n");
		b.append("return;\n");
		b.append("}\n\n");
		b.append("if(");

		for (final DTOBean dto : distinctDTOSet) {
			if (isFirstDTO)
				isFirstDTO = false;
			else
				b.append(" || ");

			b.append("selItem.getData() instanceof " + dto.getName());
		}

		b.append(")\n");
		b.append("{\n");
		b.append("event.doit = true;\n");
		b.append("dragSourceItem = selItem;\n");
		b.append("}\n");
		b.append("else\n");
		b.append("event.doit = false;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.dnd.DragSourceAdapter#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void dragSetData(DragSourceEvent event)\n");
		b.append("{\n");
		b.append("if(dragSourceItem == null)\n");
		b.append("{\n");
		b.append("event.doit = false;\n");
		b.append("return;\n");
		b.append("}\n\n");

		isFirstDTO = true;

		for (final DTOBean dto : distinctDTOSet) {
			if (isFirstDTO) {
				b.append("if");
				isFirstDTO = false;
			}
			else
				b.append("else if");

			final DTOBeanAttribute pkAttr = dto.getPKAttribute();
			final String idAttributeGetter = pkAttr.getGetterName();

			b.append("(dragSourceItem.getData() instanceof final " + dto.getName() + " dto)\n");
			b.append("event.data = " + pkAttr.getDomainAttribute().convertToString("dto." + idAttributeGetter) + ";\n");
		}

		b.append("else\n");
		b.append("event.doit = false;\n");
		b.append("}\n");
		b.append("});\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the drop listener
	 */
	private void createDropListener() {
		var b = new StringBuilder();
		var methodSignature = "void initializeDropTarget()";

		if (dropItems.isEmpty())
			return;

		b.append("/**\n");
		b.append(" * Initialize drop target listener\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var dropTarget = new DropTarget(tree, DND.DROP_MOVE);\n");

		if (project.getClientPlatform() == ClientPlatformEnumeration.RCP)
			b.append("dropTarget.setTransfer(TextTransfer.getInstance());\n\n");
		else
			b.append("dropTarget.setTransfer(new Transfer[] { TextTransfer.getInstance() });\n\n");

		b.append("dropTarget.addDropListener(new DropTargetAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void drop(DropTargetEvent event)\n");
		b.append("{\n");
		b.append("final TreeItem itemDrop = tree.getItem(tree.toControl(new Point(event.x, event.y)));\n\n");
		b.append("if(itemDrop == null)\n");
		b.append("return;\n\n");
		b.append("if(dragSourceItem == null || dragSourceItem.getData() == null)\n");
		b.append("return;\n\n");

		boolean isFirstItem = true;
		boolean needsTreeStructureMoveMethod = false;

		for (final TreeViewItem item : dropItems) {
			if (isFirstItem) {
				b.append("if");
				isFirstItem = false;
			}
			else
				b.append("else if");

			final String dtoName = item.getItemDTO().getName();
			var helperDTOName = "";

			b.append("(dragSourceItem.getData() instanceof final " + dtoName + " dragDTO && ");

			if (item.isRootItem())
				b.append("itemDrop.getData() instanceof final " + dtoName + " dropDTO)\n");
			else {
				final String assocName = item.getAssociation().getUpperCaseName();
				helperDTOName = assocName + "TreeHelperDTO";

				b.append("itemDrop.getData() instanceof final " + helperDTOName + " dropDTO)\n");
			}

			b.append("{\n");

			final var fb = new StringBuilder();
			final String pkAttrGetter = item.getItemDTO().getPKAttribute().getGetterName();
			String pkParentAttrGetter = pkAttrGetter;

			if (!item.isRootItem())
				pkParentAttrGetter = "getParentId()";

			final JavaType pkAttrType = item.getItemDTO().getPKAttribute().getDomainAttribute().getJavaType();
			final BoundaryMethod dropMethod = item.getDropMethod();
			final var declarationGenerator = new ServiceDeclarationGenerator(this, dropMethod, fb);

			declarationGenerator.addLocalVariable();

			fb.append("\n");
			fb.append("try\n");
			fb.append("{\n");

			if (item.isRootItem()) {
				final var treeHelperDTOName = item.getItemDTO().getDomainObject().getUpperCaseName() + "TreeHelperDTO";
				needsTreeStructureMoveMethod = true;

				fb.append("// Avoid cyclic reference!\n");

				if (pkAttrType.isPrimitive())
					fb.append("if(dragDTO." + pkAttrGetter + " == dropDTO." + pkParentAttrGetter + ")\n");
				else
					fb.append("if(dragDTO." + pkAttrGetter + ".equals(dropDTO." + pkParentAttrGetter + "))\n");

				fb.append("return;\n\n");
				fb.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");

				new ServiceInvocationGenerator(dropMethod, fb).addInvocation("dragDTO." + pkAttrGetter, "dropDTO." + pkParentAttrGetter);

				fb.append("\n");
				fb.append("final var parent = (" + treeHelperDTOName + ") itemDrop.getData(ITEM_DATA_KEY);\n\n");
				fb.append("if(parent.isDataLoaded())\n");
				fb.append("{\n");
				fb.append("final var newItem = new TreeItem(itemDrop, SWT.NONE);\n");
				fb.append("newItem.setText(dragSourceItem.getText());\n");
				fb.append("newItem.setData(dragSourceItem.getData());\n");
				fb.append("newItem.setImage(dragSourceItem.getImage());\n\n");
				fb.append("moveTreeItemStructure(newItem, dragSourceItem);\n");
				fb.append("}\n\n");
				fb.append("dragSourceItem.dispose();\n");
				fb.append("dragSourceItem = null;\n");
			}
			else if (item.getAssociation() instanceof OneToManyAssociation) {
				needsTreeStructureMoveMethod = true;

				fb.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");

				new ServiceInvocationGenerator(dropMethod, fb).addInvocation("dragDTO." + pkAttrGetter, "dropDTO." + pkParentAttrGetter);

				fb.append("\n");
				fb.append("final var newItem = new TreeItem(itemDrop, SWT.NONE);\n");
				fb.append("newItem.setText(dragSourceItem.getText());\n");
				fb.append("newItem.setData(dragSourceItem.getData());\n");
				fb.append("newItem.setImage(dragSourceItem.getImage());\n\n");
				fb.append("moveTreeItemStructure(newItem, dragSourceItem);\n\n");
				fb.append("dragSourceItem.dispose();\n");
				fb.append("dragSourceItem = null;\n");
			}
			else {
				fb.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_WAIT));\n\n");

				new ServiceInvocationGenerator(dropMethod, fb).addInvocation("dropDTO." + pkParentAttrGetter, "dragDTO." + pkAttrGetter);

				fb.append("\n");
				fb.append("// Refresh parent node\n");
				fb.append("add" + item.getAssociation().getUpperCaseName() + "(itemDrop, dropDTO);\n");
				fb.append("}\n");
				fb.append("catch (final DuplicateCollectionEntryException e)\n");
				fb.append("{\n");
				fb.append("MessageDialog.openWarning(parentShell, ");
				fb.append(i18n.getI18NMessage("msg_title_drop_op", "Drop operation") + ", ");
				fb.append(i18n.getI18NMessage("msg_err_duplicate_entry", "Duplicate entry in list is disallowed!") + ");\n");
			}

			fb.append("}\n");
			fb.append("catch (final Exception e)\n");
			fb.append("{\n");

			addErrorLog(fb, "Drop operation failed!", "e");

			fb.append("\n");
			fb.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_drop_op", "Drop operation") + ", ");
			fb.append(i18n.getI18NMessage("msg_err_drop_op", "Drop operation failed! Message: ") + " + e.getMessage());\n");
			fb.append("}\n");
			fb.append("finally\n");
			fb.append("{\n");
			fb.append("parentShell.setCursor(display.getSystemCursor(SWT.CURSOR_ARROW));\n");

			declarationGenerator.addCloseStatement();

			fb.append("}\n");

			if (addSecurity && dropMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(dropMethod.getRoles(), fb.toString()));
			else
				b.append(fb);

			b.append("}\n");
		}

		b.append("}\n");
		b.append("});\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (needsTreeStructureMoveMethod) {
			methodSignature = "void moveTreeItemStructure(TreeItem newItem, TreeItem oldItem)";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Perform a complete move of one item including all sub-items to another parent item\n");
			b.append(" * @param newItem\n");
			b.append(" * @param oldItem\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");
			b.append("for(final TreeItem i : oldItem.getItems())\n");
			b.append("{\n");
			b.append("final var childItem = new TreeItem(newItem, SWT.NONE);\n");
			b.append("childItem.setText(i.getText());\n");
			b.append("childItem.setData(i.getData());\n");
			b.append("childItem.setImage(i.getImage());\n\n");
			b.append("moveTreeItemStructure(childItem, i);\n");
			b.append("}\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}
	}

	/**
	 * Add the key listener method
	 */
	private void createKeyListener() {
		final var b = new StringBuilder();
		final var methodSignature = "void addKeyListener()";
		final boolean addEnterHandler = hasUpdateOrReadonlyForm();

		importPackage("org.eclipse.swt.events");

		b.append("/**\n");
		b.append(" * Add key listener to tree view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("tree.addKeyListener(new KeyAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void keyReleased(KeyEvent e)\n");
		b.append("{\n");
		b.append("final TreeItem selItem = getSelectedItem();\n\n");
		b.append("if(selItem == null || selItem.getData() == null)\n");
		b.append("return;\n\n");

		if (addEnterHandler)
			b.append(addEnterKeyHandler());

		if (hasDeleteMethod())
			b.append(addDeleteKeyHandler(addEnterHandler));

		b.append("}\n");
		b.append("});\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the part of the key listener that is responsible for handling the ENTER key
	 * @return the generated content
	 */
	private String addEnterKeyHandler() {
		final var b = new StringBuilder();
		boolean isFirstDTO = true;

		b.append("if(e.character == SWT.CR)\n");
		b.append("{\n");

		for (final DTOBean dto : distinctDTOSet) {
			final Form editForm = getEditForm(dto.getDomainObject());
			final Form viewForm = getReadOnlyForm(dto.getDomainObject());

			if (editForm != null || viewForm != null) {
				if (isFirstDTO) {
					b.append("if");

					isFirstDTO = false;
				}
				else
					b.append("else if");

				b.append("(selItem.getData() instanceof final " + dto.getName() + " dto)\n");
				b.append("{\n");

				var methodName = "";

				if (editForm != null) {
					methodName = "edit" + dto.getDomainObject().getName();

					if (viewForm != null && addSecurity) {
						b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\nreturn;\n"));

						methodName = "view" + dto.getDomainObject().getName();

						b.append("\n");
						b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), methodName + "(dto);\n"));
					}
					else
						b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\n"));
				}
				else {
					methodName = "view" + dto.getDomainObject().getName();

					b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), methodName + "(dto);\n"));
				}

				b.append("}\n");
			}
		}

		b.append("}\n");

		return b.toString();
	}

	/**
	 * Add the part of the key listener that is responsible for handling the DELETE key
	 * @param addEnterKeyHandler
	 * @return the generated content
	 */
	private String addDeleteKeyHandler(boolean addEnterKeyHandler) {
		final var b = new StringBuilder();

		if (addEnterKeyHandler)
			b.append("else ");

		b.append("if(e.character == SWT.DEL)\n");
		b.append("{\n");

		// Add the delete method for the root tree item
		final BoundaryMethod deleteMethodOfRootItem = getDeleteMethod(rootItem);
		boolean firstMethod = true;

		if (deleteMethodOfRootItem != null) {
			final var methodName = "delete" + rootItem.getItemDTO().getDomainObject().getName();
			final var fb = new StringBuilder();
			firstMethod = false;

			b.append("if(selItem.getData() instanceof final " + rootItem.getItemDTO().getName() + " dto)\n");
			b.append("{\n");

			fb.append("final boolean success = " + methodName + "(dto);\n\n");
			fb.append("if(success)\n");
			fb.append("selItem.dispose();\n");

			if (addSecurity && deleteMethodOfRootItem.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(deleteMethodOfRootItem.getRoles(), fb.toString()));
			else
				b.append(fb.toString());

			b.append("}\n");
		}

		// Add further delete methods
		for (final Map.Entry<TreeViewItem, DTOBean> entry : subTreeItemDTOMap.entrySet()) {
			BoundaryMethod deleteMethod = null;
			final BoundaryMethod removeMethod = getSubItemRemoveMethod(entry.getKey());
			final DTOBean dto = entry.getValue();

			if (removeMethod == null)
				deleteMethod = getDeleteMethod(entry.getKey());
			else {
				final var parentDTOName = entry.getKey().getAssociation().getUpperCaseName() + "TreeHelperDTO";

				if (firstMethod)
					firstMethod = false;
				else
					b.append("else ");

				b.append("if(selItem.getData() instanceof final " + dto.getName() + " item");
				b.append(" && selItem.getParentItem().getData() instanceof final " + parentDTOName + " parent)\n");
				b.append("{\n");

				final var fb = new StringBuilder();
				fb.append("final boolean success = " + removeMethod.getName() + "(parent, item);\n\n");
				fb.append("if(success)\n");
				fb.append("selItem.dispose();\n");

				if (addSecurity && removeMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
					b.append(securityHelper.wrapSecurityCode(removeMethod.getRoles(), fb.toString()));
				else
					b.append(fb.toString());

				b.append("}\n");
			}

			if (deleteMethod != null) {
				final var methodName = "delete" + dto.getDomainObject().getName();

				if (firstMethod)
					firstMethod = false;
				else
					b.append("else ");

				b.append("if(selItem.getData() instanceof final " + dto.getName() + " dto)\n");
				b.append("{\n");

				final var fb = new StringBuilder();
				fb.append("final boolean success = " + methodName + "(dto);\n\n");
				fb.append("if(success)\n");
				fb.append("selItem.dispose();\n");

				if (addSecurity && deleteMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
					b.append(securityHelper.wrapSecurityCode(deleteMethod.getRoles(), fb.toString()));
				else
					b.append(fb.toString());

				b.append("}\n");
			}
		}

		b.append("}\n");

		return b.toString();
	}

	/**
	 * Add the tree listener method
	 * @param rootItem
	 */
	private void createTreeListener(TreeViewItem rootItem) {
		final var b = new StringBuilder();
		final var methodSignature = "void addTreeListener()";

		importPackage("org.eclipse.swt.events");

		b.append("/**\n");
		b.append(" * Add tree listener to tree view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("tree.addTreeListener(new TreeAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.TreeAdapter#treeCollapsed(org.eclipse.swt.events.TreeEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void treeCollapsed(TreeEvent e)\n");
		b.append("{\n");
		b.append("final var selItem = (TreeItem) e.item;\n\n");
		b.append("if(selItem == null)\n");
		b.append("return;\n\n");
		b.append("tree.setSelection(selItem);\n");
		b.append("super.treeCollapsed(e);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.TreeAdapter#treeExpanded(org.eclipse.swt.events.TreeEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void treeExpanded(TreeEvent e)\n");
		b.append("{\n");
		b.append("final var selItem = (TreeItem) e.item;\n\n");
		b.append("if(selItem == null || selItem.getData() == null)\n");
		b.append("return;\n\n");

		boolean firstIf = true;

		for (final String assocName : helperDTOMap.keySet()) {
			final var dtoName = assocName + "TreeHelperDTO";

			if (firstIf) {
				firstIf = false;
				b.append("if");
			}
			else
				b.append("else if");

			b.append("(selItem.getData() instanceof final " + dtoName + " dto && !dto.isDataLoaded())\n");
			b.append("{\n");
			b.append("dto.setDataLoaded(true);\n");

			final var fetchMethodName = "add" + assocName.substring(0, 1).toUpperCase() + assocName.substring(1);

			b.append(fetchMethodName + "(selItem, dto);\n");
			b.append("}\n");
		}

		if (tree.getRecursiveMethod() != null) {
			if (firstIf)
				b.append("if");
			else
				b.append("else if");

			final String domainObjectName = rootItem.getItemDTO().getDomainObject().getName();
			final var dtoName = domainObjectName + "TreeHelperDTO";

			b.append("(selItem.getData(ITEM_DATA_KEY) instanceof final " + dtoName + " dto && !dto.isDataLoaded())\n");
			b.append("{\n");
			b.append("dto.setDataLoaded(true);\n");

			final var fetchMethodName = "add" + rootItem.getItemDTO().getDomainObject().getNamePlural() + "OfParent"
					+ rootItem.getItemDTO().getDomainObject().getName();

			b.append(fetchMethodName + "(selItem, dto);\n");
			b.append("}\n");
		}

		b.append("}\n");
		b.append("});\n");
		b.append("}\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the mouse listener method
	 */
	private void createMouseListener() {
		final var methodSignature = "void addMouseAndMenuListener()";
		final var b = new StringBuilder();
		boolean isFirstDTO = true;

		b.append("/**\n");
		b.append(" * Add mouse and menu listener to tree view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("tree.addMenuDetectListener(_ ->\n");
		b.append("{\n");
		b.append("final TreeItem selItem = getSelectedItem();\n");
		b.append("tree.setMenu(null);\n\n");
		b.append("if(selItem == null || selItem.getData() == null)\n");
		b.append("return;\n\n");

		for (final DTOBean dto : distinctDTOSet) {
			if (isFirstDTO) {
				b.append("if");
				isFirstDTO = false;
			}
			else
				b.append("else if");

			final var menuName = "menu" + dto.getDomainObject().getName();

			b.append("(selItem.getData() instanceof " + dto.getName() + ")\n");
			b.append("tree.setMenu(" + menuName + ");\n");
		}

		helperDTOMap.keySet().forEach(assocName -> {
			final var dtoName = assocName + "TreeHelperDTO";
			final var menuName = "menu" + assocName;

			b.append("else if(selItem.getData() instanceof " + dtoName + ")\n");
			b.append("tree.setMenu(" + menuName + ");\n");
		});

		b.append("});\n");

		if (hasUpdateOrReadonlyForm()) {
			importPackage("org.eclipse.swt.events");

			b.append("\n");
			b.append("tree.addMouseListener(new MouseAdapter()\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void mouseDoubleClick(MouseEvent e)\n");
			b.append("{\n");
			b.append("final TreeItem selItem = getSelectedItem();\n");
			b.append("tree.setMenu(null);\n\n");
			b.append("if(selItem == null || selItem.getData() == null)\n");
			b.append("return;\n\n");

			isFirstDTO = true;

			for (final DTOBean dto : distinctDTOSet) {
				final Form editForm = getEditForm(dto.getDomainObject());
				final Form viewForm = getReadOnlyForm(dto.getDomainObject());

				if (editForm != null || viewForm != null) {
					if (isFirstDTO) {
						b.append("if");
						isFirstDTO = false;
					}
					else
						b.append("else if");

					b.append("(selItem.getData() instanceof final " + dto.getName() + " dto)\n");
					b.append("{\n");

					var methodName = "";

					if (editForm != null) {
						methodName = "edit" + dto.getDomainObject().getName();

						if (viewForm != null && addSecurity) {
							b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\nreturn;\n"));

							methodName = "view" + dto.getDomainObject().getName();

							b.append("\n");
							b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), methodName + "(dto);\n"));
						}
						else
							b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\n"));
					}
					else {
						methodName = "view" + dto.getDomainObject().getName();

						b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), methodName + "(dto);\n"));
					}

					b.append("}\n");
				}
			}

			b.append("}\n");
			b.append("});\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
