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
package net.codecadenza.eclipse.generator.client.imp.javafx.view;

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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.file.JavaFXFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.security.JavaFXSecurityHelper;
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
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;

/**
 * <p>
 * Generator for tree views of a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXTreeViewGenerator extends AbstractTreeViewGenerator {
	private static final String GROUP_SUFFIX = "_GROUP";
	private static final String ROOT_TREE_ITEM = "tree";

	private final RichClientI18NGenerator i18n;
	private final JavaFXSecurityHelper securityHelper;
	private final HashMap<String, String> menuMap = new HashMap<>();
	private final HashSet<String> groupSet = new HashSet<>();

	/**
	 * Constructor
	 * @param tree
	 */
	public JavaFXTreeViewGenerator(TreeView tree) {
		super(tree);

		this.securityHelper = new JavaFXSecurityHelper(project);
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
		importPackage("java.util");
		importPackage("javafx.scene.control");
		importPackage("net.codecadenza.runtime.richclient.javafx.image");
		importPackage("net.codecadenza.runtime.richclient.javafx.tree");

		addImports(securityHelper.getSecurityImports());

		if (addQuickSearch) {
			importPackage("javafx.scene");
			importPackage("javafx.scene.layout");
			importPackage("javafx.geometry");
		}

		if (addAdvSearch)
			importPackage("net.codecadenza.runtime.richclient.search.util");

		if (addQuickSearch || addAdvSearch)
			importPackage("net.codecadenza.runtime.search.dto");

		if (recursiveStructure)
			importPackage("net.codecadenza.runtime.richclient.javafx.dialog");

		// Add all DTO imports
		importPackage(rootItem.getItemDTO().getNamespace().toString());

		addSubItemDTOImports(rootItem.getChildren());

		addSubItemBoundaryImports(rootItem.getChildren());

		// Check if the imports for download operations are necessary
		if (!getDownloadMethods(rootItem).isEmpty()) {
			importPackage("javafx.stage");
			importClass("java.io.File");

			if (project.isJavaSEApplication())
				importPackage("net.codecadenza.runtime.file");
		}

		if (!dropItems.isEmpty())
			importPackage("net.codecadenza.runtime.richclient.javafx.dialog");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + tree.getName() + " extends AbstractTreeView<" + tree.getDTO().getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final DomainObject rootDomainObject = rootItem.getItemDTO().getDomainObject();
		final var menuName = "menu" + rootDomainObject.getName();

		addProtectedConstant(JavaType.STRING, "ITEM_LABEL_SEPARATOR", "\": \"").create();
		addPrivateField("ContextMenu", menuName).create();

		addTreeGroups();

		menuMap.put(menuName, rootDomainObject.getName().toUpperCase() + GROUP_SUFFIX);

		// Add all context menu declarations for the sub-items
		getMenuDeclarations(rootItem.getChildren());

		// Add the action to create a new object
		if (getCreateNewForm(rootItem) != null) {
			final var actionName = "actionCreate" + rootDomainObject.getName();

			addPrivateField("Create" + rootDomainObject.getName() + "Action", actionName).create();
		}

		// Add fields for performing a quick-search!
		if (addQuickSearch)
			tree.getQuickSearchItems().forEach(a -> {
				final DTOBeanAttribute attr = a.getDTOAttribute();
				final var fieldName = "txt" + attr.getUpperCaseName();

				addPrivateField("TextField", fieldName).create();
			});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		if (addQuickSearch)
			addQuickSearchMethod();

		// Add count method
		if (addAdvSearch)
			addCountMethod();

		// Add tree item methods
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

		// Add common tree view methods
		addCommonMethods();

		addCreateAction();

		// Add the key listener method
		if (addKeyListener())
			createKeyListener();

		// Add the drop target listener method
		createDropListener();

		// Add the mouse listener
		if (hasUpdateOrReadonlyForm())
			createMouseListener();

		// Add comparators
		addComparatorClasses(rootItem);

		// Add the method that implements getLogger()
		addGetLoggerMethod("net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView");

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

		items.forEach(item -> {
			// Add imports for downloading files
			if (!getDownloadMethods(item).isEmpty()) {
				importPackage("javafx.stage");
				importClass("java.io.File");

				if (project.isJavaSEApplication())
					importPackage("net.codecadenza.runtime.file");
			}

			if (item.getAssociation() instanceof ManyToManyAssociation)
				importClass("net.codecadenza.runtime.repository.DuplicateCollectionEntryException");

			addSubItemBoundaryImports(item.getChildren());
		});
	}

	/**
	 * Recursive method to add declarations for context menus
	 * @param items
	 */
	private void getMenuDeclarations(Collection<TreeViewItem> items) {
		for (final TreeViewItem item : items) {
			final DomainObject domainObject = item.getItemDTO().getDomainObject();
			var menuName = "menu" + domainObject.getName();

			menuMap.computeIfAbsent(menuName, fieldName -> {
				addPrivateField("ContextMenu", fieldName).create();

				return domainObject.getName().toUpperCase() + GROUP_SUFFIX;
			});

			// Add the context menu for the parent of this item
			final String assocName = item.getAssociation().getUpperCaseName();

			menuName = "menu" + assocName;

			menuMap.put(menuName, item.getAssociation().getName().toUpperCase() + GROUP_SUFFIX);

			addPrivateField("ContextMenu", menuName).create();

			getMenuDeclarations(item.getChildren());
		}
	}

	/**
	 * Recursive method to add all tree groups
	 * @param items
	 */
	private void addTreeGroups(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			addTreeGroups(item);
			addTreeGroups(item.getChildren());
		});
	}

	/**
	 * Create the tree groups
	 * @param item
	 */
	private void addTreeGroups(TreeViewItem item) {
		final String assocName = item.getAssociation().getUpperCaseName();
		String groupName = assocName.toUpperCase() + GROUP_SUFFIX;

		if (!groupSet.contains(groupName)) {
			groupSet.add(groupName);

			addPrivateConstant(JavaType.STRING, groupName, "\"" + groupName + "\"").create();
		}

		groupName = item.getItemDTO().getDomainObject().getName().toUpperCase() + GROUP_SUFFIX;

		if (!groupSet.contains(groupName)) {
			groupSet.add(groupName);

			addPrivateConstant(JavaType.STRING, groupName, "\"" + groupName + "\"").create();
		}
	}

	/**
	 * Create the tree groups
	 */
	private void addTreeGroups() {
		final String groupName = tree.getDomainObject().getName().toUpperCase() + GROUP_SUFFIX;

		groupSet.add(groupName);

		addPrivateConstant(JavaType.STRING, groupName, "\"" + groupName + "\"").create();

		addTreeGroups(rootItem.getChildren());
	}

	/**
	 * Add the count method
	 */
	private void addCountMethod() {
		final var b = new StringBuilder();
		final BoundaryMethod countMethod = tree.getCountMethod();
		final var methodSignature = "long countData()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#countData()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, countMethod, b);
		declarationGenerator.addLocalVariable();

		b.append("\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("try\n");
			b.append("{\n");
		}

		b.append("return ");

		new ServiceInvocationGenerator(countMethod, b).addInvocation("searchObj");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method to perform a quick-search
	 */
	private void addQuickSearchMethod() {
		final var b = new StringBuilder();
		final var methodSignature = "Node getQuickSearchBar()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#getQuickSearchBar()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var panFilter = new GridPane();\n");
		b.append("panFilter.setHgap(5);\n");
		b.append("panFilter.setVgap(5);\n\n");
		b.append("final var cmdSearch = new Button(" + i18n.getI18NMessage("cmd_ok", "OK") + ");\n");
		b.append("cmdSearch.setOnAction(_ ->\n");
		b.append("{\n");
		b.append("searchObj = new SearchDTO();\n");
		b.append("searchObj.setDateFormat(userFormat.getDateFormat());\n");
		b.append("searchObj.setDateTimeFormat(userFormat.getDateTimeFormat());\n");
		b.append("searchObj.setNumberFormat(userFormat.getDecimalFormat());\n");
		b.append("searchObj.setCaseSensitive(false);\n");
		b.append("searchObj.setExactFilterMatch(true);\n");
		b.append("searchObj.setCount(false);\n");
		b.append("searchObj.setMaxResult(1000);\n\n");

		int fieldIndex = 1;

		for (final TreeSearchItem a : tree.getQuickSearchItems()) {
			final var fieldName = "txt" + a.getDTOAttribute().getUpperCaseName();
			final var searchFieldName = "field" + fieldIndex;

			b.append("if(!" + fieldName + ".getText().isEmpty())\n");
			b.append("{\n");
			b.append("final var " + searchFieldName + " = new SearchFieldDTO(");
			b.append((fieldIndex - 1) + ", " + a.getDTOAttribute().getSelectTokenConstant() + ", ");
			b.append(i18n.getI18N(a.getDTOAttribute(), a.getLabel()) + ", SearchFieldDataTypeEnum.STRING, 80);\n");
			b.append(searchFieldName + ".setFilterCriteria(" + fieldName + ".getText());\n\n");
			b.append("searchObj.getSearchFields().add(" + searchFieldName + ");\n");
			b.append("}\n\n");

			fieldIndex++;
		}

		b.append("refreshView();\n");
		b.append("});\n\n");

		int rowIndex = 0;

		for (final TreeSearchItem s : tree.getQuickSearchItems()) {
			final var txtName = "txt" + s.getDTOAttribute().getUpperCaseName();

			b.append("panFilter.add(new Label(" + i18n.getI18N(s.getDTOAttribute(), s.getLabel(), true) + "), 0, " + rowIndex + ");\n");
			b.append("panFilter.add(" + txtName + " = new TextField(), 1, " + rowIndex + ");\n");

			rowIndex++;
		}

		b.append("panFilter.add(cmdSearch, 0, " + rowIndex + ");\n");
		b.append("panFilter.setPadding(new Insets(5, 5, 5, 5));\n\n");

		tree.getQuickSearchItems().forEach(item -> {
			final var txtName = "txt" + item.getDTOAttribute().getUpperCaseName();

			b.append(txtName + ".onActionProperty().bind(cmdSearch.onActionProperty());\n");
		});

		b.append("\n");
		b.append("return panFilter;\n");
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
		final String label = itemDomainObject.getLabel();
		final DTOBeanAttribute pkDTOAttribute = treeItem.getItemDTO().getPKAttribute();
		final String pkGetter = pkDTOAttribute.getGetterName();
		BoundaryMethod deleteMethod = null;
		StringBuilder b;

		// We have to check if we delete an object or if we remove an object from an association list!
		final BoundaryMethod removeMethod = getSubItemRemoveMethod(treeItem);

		if (removeMethod != null) {
			final String refPKGetter = treeItem.getItemDTO().getPKAttribute().getGetterName();
			final String parentPKType = treeItem.getAssociation().getDomainObject().getPKAttribute().getJavaType().getName();
			final var methodSignature = "boolean " + removeMethod.getName() + "(" + parentPKType + " parentId, "
					+ treeItem.getItemDTO().getName() + " item)";
			final String assocName = treeItem.getAssociation().getName();

			if (!methodSet.contains(methodSignature)) {
				final var msg = "Do you really want to remove selected object from list?";

				methodSet.add(methodSignature);
				importPackage("net.codecadenza.runtime.richclient.javafx.dialog");

				b = new StringBuilder();
				b.append(removeMethod.generateBeginOfJavadocComment());
				b.append(" * @param parentId\n");
				b.append(" * @param item\n");
				b.append(" * @return true if operation was finished successfully!\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("final String msg = " + i18n.getI18NMessage("msg_confirm_remove_item", msg) + ";\n");
				b.append("final String title = " + i18n.getI18NMessage("msg_title_remove_item", "Remove item") + ";\n\n");
				b.append("if(DialogButtonType.YES != DialogUtil.openConfirmationDialog(null, title, msg))\n");
				b.append("return false;\n\n");

				final var declarationGenerator = new ServiceDeclarationGenerator(this, removeMethod, b);
				declarationGenerator.addLocalVariable();

				b.append("\n");
				b.append("try\n");
				b.append("{\n");

				final var logMsg = "Remove selected object with id '{}' from list '" + assocName + "' of parent object with id '{}'";

				addDebugLog(b, logMsg, "item." + refPKGetter, "parentId");

				b.append("\n");

				new ServiceInvocationGenerator(removeMethod, b).addInvocation("parentId", "item." + refPKGetter);

				b.append("return true;\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while removing selected object!", "e");

				b.append("\n");
				b.append("DialogUtil.openErrorDialog(null, title, e);\n");
				b.append("return false;\n");
				b.append("}\n");

				declarationGenerator.addCloseStatementInFinallyBlock();

				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		if (removeMethod == null)
			deleteMethod = getDeleteMethod(treeItem);

		if (deleteMethod != null) {
			final var methodSignature = "boolean delete" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName()
					+ " item)";

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);
				importPackage("net.codecadenza.runtime.richclient.javafx.dialog");

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Delete " + label + "\n");
				b.append(" * @param item\n");
				b.append(" * @return true if operation was finished successfully!\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("final String msg = ");
				b.append(i18n.getI18NMessage("msg_confirm_delete", "Do you really want to delete selected object?") + ";\n");
				b.append("final String title = " + i18n.getI18NMessage("msg_title_delete", "Delete object?") + ";\n\n");
				b.append("if(DialogButtonType.YES != DialogUtil.openConfirmationDialog(null, title, msg))\n");
				b.append("return false;\n\n");

				final var declarationGenerator = new ServiceDeclarationGenerator(this, deleteMethod, b);
				declarationGenerator.addLocalVariable();

				b.append("\n");
				b.append("try\n");
				b.append("{\n");

				addDebugLog(b, "Delete selected object with id '{}'", "item." + pkGetter);

				b.append("\n");

				new ServiceInvocationGenerator(deleteMethod, b).addInvocation("item." + pkGetter);

				b.append("return true;\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while deleting selected object!", "e");

				b.append("\n");
				b.append("DialogUtil.openErrorDialog(null, title, e);\n");
				b.append("return false;\n");
				b.append("}\n");

				declarationGenerator.addCloseStatementInFinallyBlock();

				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		// Check if a form of type 'ADD' exists
		Form addForm = null;

		if (treeItem.isRootItem() && tree.getRecursiveMethod() != null)
			addForm = getAddForm(treeItem);

		if (addForm != null) {
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

			final var methodSignature = "boolean add" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " item)";

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Add " + label + "\n");
				b.append(" * @param item\n");
				b.append(" * @return true if operation was finished successfully!\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("final var dlg = new " + addForm.getName() + "(null, item." + pkGetter + ");\n\n");
				b.append("return DialogButtonType.OK == dlg.open();\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		// Check if a form of type 'UPDATE' exists
		final Form editForm = getEditForm(itemDomainObject);

		if (editForm != null) {
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

			final var methodSignature = "boolean edit" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " item)";

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Edit selected " + label + "\n");
				b.append(" * @param item\n");
				b.append(" * @return true if operation was finished successfully!\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("final var dlg = new " + editForm.getName() + "(null, item." + pkGetter + ");\n\n");
				b.append("return DialogButtonType.OK == dlg.open();\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		// Check if a form of type 'READONLY' exists
		final Form viewForm = getReadOnlyForm(itemDomainObject);

		if (viewForm != null) {
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

			final var methodSignature = "void view" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " item)";

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * View selected " + label + "\n");
				b.append(" * @param item\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("new " + viewForm.getName() + "(null, item." + pkGetter + ").open();\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		for (final DomainAttribute attr : itemDomainObject.getAllLobAttributes()) {
			final BoundaryMethod downloadMethod = getDownloadMethod(itemDomainObject, attr);
			final String methodName;

			if (downloadMethod == null)
				continue;

			if (attr.getDomainObject().equals(itemDomainObject))
				methodName = "download" + attr.getUpperCaseName();
			else
				methodName = "download" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

			final var methodSignature = "void " + methodName + "(final " + treeItem.getItemDTO().getName() + " item)";
			final String typeName = treeItem.getItemDTO().getPKAttribute().getDomainAttribute().getJavaType().getName();
			final String getter = treeItem.getItemDTO().getPKAttribute().getGetterName();

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
				b.append("/**\n");
				b.append(" * Download " + attr.getLabel() + "\n");
				b.append(" * @param item\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("private " + methodSignature + "\n");
				b.append("{\n");
				b.append("final " + typeName + " id = item." + getter + ";\n");
				b.append(new JavaFXFileHandlingGenerator(this, downloadMethod, i18n).createDownloadMethodBody(true, "null"));
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator#addTreeParentMenuMethods(java.util.
	 * Collection)
	 */
	@Override
	protected void addTreeParentMenuMethods(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			final String assocName = item.getAssociation().getUpperCaseName();
			final var menuMethodName = "initialize" + assocName + "Menu";
			final var methodSignature = "void " + menuMethodName + "()";
			final var b = new StringBuilder();

			menuMethodInitializerSet.add(menuMethodName);

			importPackage("javafx.scene.image");

			b.append("/**\n");
			b.append(" * Initialize menu\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");
			b.append("menu" + assocName + " = new ContextMenu();\n\n");
			b.append("// Add menu item to refresh sub-items\n");
			b.append("final var itemRefresh = new MenuItem(" + i18n.getI18NMessage("action_name_refresh", "Refresh") + ");\n");
			b.append("itemRefresh.setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_REFRESH)));\n");
			b.append("itemRefresh.setOnAction(_ -> \n");
			b.append("{\n");
			b.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
			b.append("return;\n\n");
			b.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
			b.append("add" + assocName + "(selectedItem);\n");
			b.append("});\n\n");
			b.append("menu" + assocName + ".getItems().add(itemRefresh);\n");

			// Check if a form of type 'ADD' exists
			if (item.getAssociation() instanceof OneToManyAssociation) {
				final Form addForm = getAddForm(item);

				if (addForm != null) {
					final var methodName = "add" + item.getItemDTO().getDomainObject().getName() + "To" + assocName;
					final JavaType pkType = item.getAssociation().getDomainObject().getPKAttribute().getJavaType();
					final var s = new StringBuilder();

					b.append("\n");
					s.append("// Add menu item to add " + item.getItemDTO().getDomainObject().getLabel() + "\n");
					s.append("final var itemAdd = new MenuItem(" + i18n.getI18NMessage("action_name_add", "Add") + ");\n");
					s.append("itemAdd.setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_NEW_DATA)));\n");
					s.append("itemAdd.setOnAction(_ -> \n");
					s.append("{\n");
					s.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
					s.append("return;\n\n");
					s.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
					s.append("if(!(selectedItem.getData() instanceof final " + pkType.getWrapperTypeName() + " parentId))\n");
					s.append("return;\n\n");
					s.append("if(" + methodName + "(parentId) && selectedItem.isDataLoaded())\n");
					s.append("add" + assocName + "(selectedItem);\n");
					s.append("});\n\n");
					s.append("menu" + assocName + ".getItems().add(itemAdd);\n");

					b.append(securityHelper.wrapSecurityCode(addForm.getRoles(), s.toString()));
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
		boolean isFirstAttribute = true;
		boolean isSecondAttribute = false;
		String groupName;

		b.append("itemText.setLength(0);\n");

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

			final String getter = "i." + attr.getGetterName();
			final boolean addNullCheck = !attr.getSearchType().isPrimitive();

			if (addNullCheck)
				b.append("\nif(" + getter + " != null)\n");

			b.append("itemText.append(" + attr.getDomainAttribute().convertToString(getter) + ");\n");

			if (addNullCheck)
				b.append("\n");
		}

		if (item.getDisplayAttributes().size() > 1)
			b.append("itemText.append(\")\");\n");

		b.append("\n");
		b.append("final var " + itemName + " = new TreeDataItem(i, itemText.toString(), ");

		if (item.isRootItem()) {
			groupName = tree.getDomainObject().getName().toUpperCase() + GROUP_SUFFIX;

			b.append("ImageLoader.getImage(ImageLoader.IMG_FOLDER));\n");
		}
		else {
			groupName = item.getItemDTO().getDomainObject().getName().toUpperCase() + GROUP_SUFFIX;

			b.append("ImageLoader.getImage(ImageLoader.IMG_TREE_DATA));\n");
		}

		b.append(itemName + ".setGroupName(" + groupName + ");\n");

		if (item.isRootItem() && tree.getRecursiveMethod() != null) {
			final DomainObject treeDomainObject = tree.getDomainObject();
			final var fetchMethodName = "add" + treeDomainObject.getNamePlural() + "OfParent" + treeDomainObject.getName();

			if (item.getNodes().isEmpty() && item.getChildren().isEmpty())
				b.append(itemName + ".getChildren().add(new TreeDataItem(null, null));\n");

			b.append(itemName + ".expandedProperty().addListener(_ ->\n");
			b.append("{\n");
			b.append("if(" + itemName + ".isDataLoaded())\n");
			b.append("return;\n\n");
			b.append(fetchMethodName + "(" + itemName + ");\n");
			b.append(itemName + ".setDataLoaded(true);\n");
			b.append("});\n");
		}

		b.append("\n");

		if (ROOT_TREE_ITEM.equals(parentItemName))
			b.append("rootItem.getChildren().add(" + itemName + ");\n");
		else
			b.append(parentItemName + ".getChildren().add(" + itemName + ");\n");

		// Add item nodes
		item.getNodes().forEach(node -> {
			var nodeName = "item" + node.getDTOAttribute().getUpperCaseName();
			final JavaType type = node.getDTOAttribute().getSearchType();
			final String getter = "i." + node.getDTOAttribute().getGetterName();
			final boolean addNullCheck = !type.isPrimitive();

			if (addNullCheck) {
				b.append("\nif(" + getter + " != null)\n");
				b.append("{");
			}

			if (node.getDTOAttribute().getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE) {
				final var parentNodeName = "parent" + node.getDTOAttribute().getUpperCaseName();

				b.append("\nfinal var " + parentNodeName + " = new TreeDataItem(");
				b.append(i18n.getI18N(node.getDTOAttribute(), node.getLabel()));
				b.append(", ImageLoader.getImage(ImageLoader.IMG_FOLDER));\n\n");
				b.append("for(final var element : " + getter + ")\n");
				b.append("{\n");
				b.append("final var " + nodeName + " = new TreeDataItem(");
				b.append(node.getDTOAttribute().getDomainAttribute().convertToString("element") + ", ");
				b.append("ImageLoader.getImage(ImageLoader.IMG_TREE_DATA));\n\n");
				b.append(parentNodeName + ".getChildren().add(" + nodeName + ");\n");
				b.append("}\n");

				nodeName = parentNodeName;
			}
			else if (type.isBoolean()) {
				b.append("\n");
				b.append("TreeDataItem " + nodeName + ";\n\n");
				b.append("if(" + getter + ")\n");
				b.append(nodeName + " = new TreeDataItem(" + i18n.getI18N(node.getDTOAttribute(), node.getLabel()) + ", ");
				b.append("ImageLoader.getImage(ImageLoader.IMG_CHECKED));\n");
				b.append("else\n");
				b.append(nodeName + " = new TreeDataItem(" + i18n.getI18N(node.getDTOAttribute(), node.getLabel()) + ", ");
				b.append("ImageLoader.getImage(ImageLoader.IMG_UNCHECKED));\n");
			}
			else {
				if (type instanceof final JavaEnum javaEnum)
					javaEnum.getEnumerationValues().forEach(i18n::getI18N);

				b.append("\nfinal var " + nodeName + " = new TreeDataItem(");
				b.append(i18n.getI18N(node.getDTOAttribute(), node.getLabel()) + " + ITEM_LABEL_SEPARATOR + ");
				b.append(node.getDTOAttribute().getDomainAttribute().convertToString(getter));

				if (type.isTemporalType())
					b.append(", ImageLoader.getImage(ImageLoader.IMG_CALENDAR));\n");
				else
					b.append(", ImageLoader.getImage(ImageLoader.IMG_TREE_DATA));\n");
			}

			b.append("\n");
			b.append(itemName + ".getChildren().add(" + nodeName + ");\n");

			if (addNullCheck)
				b.append("}\n");
		});

		// Add parent items for all sub-items
		for (final TreeViewItem treeItem : item.getChildren()) {
			final var subParentItemName = "item" + treeItem.getAssociation().getUpperCaseName();
			final var fetchMethodName = "add" + treeItem.getAssociation().getUpperCaseName();
			final var msgKey = tree.getName() + "_" + treeItem.getAssociation().getName();
			groupName = treeItem.getAssociation().getName().toUpperCase() + GROUP_SUFFIX;

			b.append("\n");
			b.append("final var " + subParentItemName + " = new TreeDataItem(i." + item.getItemDTO().getPKAttribute().getGetterName());
			b.append(", " + i18n.getI18NMessage(msgKey, treeItem.getLabel()) + ", ");
			b.append("ImageLoader.getImage(ImageLoader.IMG_FOLDER));\n");
			b.append(subParentItemName + ".setGroupName(" + groupName + ");\n");
			b.append(subParentItemName + ".getChildren().add(new TreeDataItem(null, null));\n");
			b.append(subParentItemName + ".expandedProperty().addListener(_ ->\n");
			b.append("{\n");
			b.append("if(" + subParentItemName + ".isDataLoaded())\n");
			b.append("return;\n\n");
			b.append(fetchMethodName + "(" + subParentItemName + ");\n");
			b.append(subParentItemName + ".setDataLoaded(true);\n");
			b.append("});\n\n");
			b.append(itemName + ".getChildren().add(" + subParentItemName + ");\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator#addItemFetchMethods(java.util.Collection)
	 */
	@Override
	protected void addItemFetchMethods(Collection<TreeViewItem> items) {
		items.forEach(item -> {
			final String assocName = item.getAssociation().getUpperCaseName();
			final var fetchMethodName = "add" + item.getAssociation().getUpperCaseName();
			final JavaType pkType = item.getAssociation().getDomainObject().getPKAttribute().getJavaType();
			final BoundaryMethod m = item.getDataFetchMethod();
			final var compName = assocName + "Comparator";
			var methodSignature = "void " + fetchMethodName + "(TreeDataItem parentItem)";

			importPackage("net.codecadenza.runtime.richclient.javafx.dialog");

			var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Add " + item.getItemDTO().getDomainObject().getLabel() + " items to parent\n");
			b.append(" * @param parentItem\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");

			addDebugLog(b, "Perform data fetch operation for " + item.getItemDTO().getDomainObject().getLabel() + " sub-items");

			b.append("\n");

			final var declarationGenerator = new ServiceDeclarationGenerator(this, m, b);
			declarationGenerator.addLocalVariable();

			b.append("\ntry\n");
			b.append("{\n");
			b.append("if(!(parentItem.getData() instanceof final " + pkType.getWrapperTypeName() + " parentId))\n");
			b.append("return;\n\n");
			b.append("statusBar.showProgress();\n\n");
			b.append("// Remove all existing items from parent tree item\n");
			b.append("parentItem.getChildren().clear();\n\n");
			b.append("final List<" + item.getItemDTO().getName() + "> items = ");

			new ServiceInvocationGenerator(m, b).addInvocation("parentId");

			b.append("Collections.sort(items, new " + compName + "());\n");
			b.append("final var itemText = new StringBuilder();\n\n");
			b.append("for(final " + item.getItemDTO().getName() + " i : items)\n");
			b.append("{\n");
			b.append(addTreeItems("parentItem", item));
			b.append("}\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while fetching data!", "e");

			b.append("\n");
			b.append("final String title = " + i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data!") + ";\n");
			b.append("statusBar.stopProgress();\n\n");
			b.append("DialogUtil.openErrorDialog(null, title, e);\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");

			declarationGenerator.addCloseStatement();

			b.append("statusBar.stopProgress();\n");
			b.append("}\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());

			// Check if a form of type 'ADD' exists
			if (item.getAssociation() instanceof OneToManyAssociation) {
				final TreeViewItem parentItem = item.getParentItem();
				final Form addForm = getAddForm(item);

				if (addForm != null) {
					final var methodName = "add" + item.getItemDTO().getDomainObject().getName() + "To" + assocName;

					importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

					methodSignature = "boolean " + methodName + "(" + pkType.getName() + " parentId)";

					b = new StringBuilder();
					b.append("/**\n");
					b.append(" * Add " + item.getItemDTO().getDomainObject().getLabel() + " to ");
					b.append(parentItem.getItemDTO().getDomainObject().getLabel() + "\n");
					b.append(" * @param parentId\n");
					b.append(" * @return true if operation was finished successfully!\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private " + methodSignature + "\n");
					b.append("{\n");
					b.append("final var dlg = new " + addForm.getName() + "(null, parentId);\n\n");
					b.append("return DialogButtonType.OK == dlg.open();\n");
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
		final JavaType pkType = treeItem.getItemDTO().getPKAttribute().getDomainAttribute().getJavaType();

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
		b.append("menu" + itemDomainObject.getName() + " = new ContextMenu();\n");

		// Check if a form of type 'ADD' exists
		Form addForm = null;
		String fetchMethodName = null;

		if (treeItem.isRootItem() && tree.getRecursiveMethod() != null) {
			final DomainObject domainObj = treeItem.getItemDTO().getDomainObject();

			fetchMethodName = "add" + domainObj.getNamePlural() + "OfParent" + domainObj.getName();
			addForm = getAddForm(treeItem);
		}

		if (addForm != null) {
			final var s = new StringBuilder();

			importPackage("javafx.scene.image");

			b.append("\n");
			s.append("// Add menu item to add a new item\n");
			s.append("final var itemAdd = new MenuItem(" + i18n.getI18NMessage("action_name_add", "Add") + ");\n");
			s.append("itemAdd.setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_NEW_DATA)));\n");
			s.append("itemAdd.setOnAction(_ -> \n");
			s.append("{\n");
			s.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
			s.append("return;\n\n");
			s.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");

			if (treeItem.equals(rootItem)) {
				s.append("if(!(selectedItem.getData() instanceof final " + treeItem.getItemDTO().getName() + " parent))\n");
				s.append("return;\n\n");
				s.append("if(add" + itemDomainObject.getName() + "(parent))\n");
			}
			else {
				s.append("if(!(selectedItem.getData() instanceof final " + pkType.getWrapperTypeName() + " parentId))\n");
				s.append("return;\n\n");
				s.append("if(add" + itemDomainObject.getName() + "(parentId))\n");
			}

			s.append("{\n");
			s.append("if(!selectedItem.isDataLoaded())\n");
			s.append("return;\n\n");
			s.append(fetchMethodName + "(selectedItem);\n");
			s.append("selectedItem.setDataLoaded(true);\n");
			s.append("}\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".getItems().add(itemAdd);\n");

			b.append(securityHelper.wrapSecurityCode(addForm.getRoles(), s.toString()));
		}

		// Check if a form of type 'UPDATE' exists
		final Form editForm = getEditForm(treeItem);

		if (editForm != null) {
			final var s = new StringBuilder();

			importPackage("javafx.scene.image");

			b.append("\n");
			s.append("// Add menu item to edit item\n");
			s.append("final var itemEdit = new MenuItem(" + i18n.getI18NMessage("action_name_edit", "Edit") + ");\n");
			s.append("itemEdit.setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_EDIT_DATA)));\n");
			s.append("itemEdit.setOnAction(_ -> \n");
			s.append("{\n");
			s.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
			s.append("return;\n\n");
			s.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
			s.append("if(!(selectedItem.getData() instanceof final " + treeItem.getItemDTO().getName() + " item))\n");
			s.append("return;\n\n");
			s.append("edit" + itemDomainObject.getName() + "(item);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".getItems().add(itemEdit);\n");

			b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), s.toString()));
		}

		// Check if a form of type 'READONLY' exists
		final Form viewForm = getReadOnlyForm(treeItem);

		if (viewForm != null) {
			final var s = new StringBuilder();

			importPackage("javafx.scene.image");

			b.append("\n");
			s.append("// Add menu item to view item\n");
			s.append("final var itemView = new MenuItem(" + i18n.getI18NMessage("action_name_view", "View") + ");\n");
			s.append("itemView.setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_VIEW_DATA)));\n");
			s.append("itemView.setOnAction(_ -> \n");
			s.append("{\n");
			s.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
			s.append("return;\n\n");
			s.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
			s.append("if(!(selectedItem.getData() instanceof final " + treeItem.getItemDTO().getName() + " item))\n");
			s.append("return;\n\n");
			s.append("view" + itemDomainObject.getName() + "(item);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".getItems().add(itemView);\n");

			b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), s.toString()));
		}

		// Add context menu items for file download operations
		for (final DomainAttribute attr : itemDomainObject.getAllLobAttributes()) {
			final BoundaryMethod downloadMethod = getDownloadMethod(itemDomainObject, attr);
			var methodName = "";

			if (downloadMethod == null)
				continue;

			importPackage("javafx.scene.image");

			if (attr.getDomainObject().equals(itemDomainObject))
				methodName = "download" + attr.getUpperCaseName();
			else
				methodName = "download" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

			final var menuItemName = methodName + "Item";
			final var s = new StringBuilder();

			b.append("\n");
			s.append("// Add menu item to download " + attr.getLabel() + "\n");
			s.append("final var " + menuItemName + " = new MenuItem(");
			s.append(i18n.getI18NMessage("action_name_download", "Download") + ");\n");
			s.append(menuItemName + ".setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_DOWNLOAD)));\n");
			s.append(menuItemName + ".setOnAction(_ -> \n");
			s.append("{\n");
			s.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
			s.append("return;\n\n");
			s.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
			s.append("if(!(selectedItem.getData() instanceof final " + treeItem.getItemDTO().getName() + " item))\n");
			s.append("return;\n\n");
			s.append(methodName + "(item);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".getItems().add(" + menuItemName + ");\n");

			if (addSecurity && downloadMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(downloadMethod.getRoles(), s.toString()));
			else
				b.append(s.toString());
		}

		// Check if either a delete or remove method should be added
		BoundaryMethod deleteMethod = null;
		BoundaryMethod removeMethod = null;

		if (treeItem.isRootItem() || treeItem.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional())
			deleteMethod = getDeleteMethod(treeItem);
		else
			removeMethod = getSubItemRemoveMethod(treeItem);

		if (deleteMethod != null) {
			final var s = new StringBuilder();

			importPackage("javafx.scene.image");

			b.append("\n");
			s.append("// Add menu item to delete an item\n");
			s.append("final var itemDelete = new MenuItem(" + i18n.getI18NMessage("action_name_delete", "Delete") + ");\n");
			s.append("itemDelete.setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_DELETE)));\n");
			s.append("itemDelete.setOnAction(_ -> \n");
			s.append("{\n");
			s.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
			s.append("return;\n\n");
			s.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
			s.append("if(!(selectedItem.getData() instanceof final " + treeItem.getItemDTO().getName() + " item))\n");
			s.append("return;\n\n");
			s.append("final var parentItem = (TreeDataItem) selectedItem.getParent();\n");
			s.append("final boolean success = delete" + itemDomainObject.getName() + "(item);\n\n");
			s.append("if(success)\n");
			s.append("parentItem.getChildren().remove(selectedItem);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".getItems().add(itemDelete);\n");

			if (addSecurity && deleteMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(deleteMethod.getRoles(), s.toString()));
			else
				b.append(s.toString());
		}

		if (removeMethod != null) {
			final JavaType pkRefType = treeItem.getAssociation().getDomainObject().getPKAttribute().getJavaType();
			final var s = new StringBuilder();

			importPackage("javafx.scene.image");

			b.append("\n");
			s.append("// Add menu item to remove item from collection\n");
			s.append("final var itemRemove = new MenuItem(" + i18n.getI18NMessage("action_name_remove_item", "Remove item") + ");\n");
			s.append("itemRemove.setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_DELETE)));\n");
			s.append("itemRemove.setOnAction(_ -> \n");
			s.append("{\n");
			s.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
			s.append("return;\n\n");
			s.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
			s.append("if(!(selectedItem.getData() instanceof final " + treeItem.getItemDTO().getName() + " item))\n");
			s.append("return;\n\n");
			s.append("final var parentItem = (TreeDataItem) selectedItem.getParent();\n\n");
			s.append("if(!(parentItem.getData() instanceof final " + pkRefType.getWrapperTypeName() + " parentId))\n");
			s.append("return;\n\n");
			s.append("final boolean success = " + removeMethod.getName() + "(parentId, item);\n\n");
			s.append("if(success)\n");
			s.append("parentItem.getChildren().remove(selectedItem);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".getItems().add(itemRemove);\n");

			if (addSecurity && removeMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(removeMethod.getRoles(), s.toString()));
			else
				b.append(s.toString());
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the methods for root tree items
	 */
	private void addCommonMethods() {
		final BoundaryBean boundaryBean = tree.getBoundaryMethod().getBoundaryBean();
		var b = new StringBuilder();
		var methodSignature = "void addRootTreeItems(List<" + tree.getDTO().getName() + "> data)";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#addRootTreeItems(java.util.List)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var itemText = new StringBuilder();\n\n");
		b.append("final var rootItem = new TreeDataItem(null, null);\n");
		b.append("rootItem.setExpanded(true);\n\n");
		b.append("treeView.setRoot(rootItem);\n\n");
		b.append("for(final " + rootItem.getItemDTO().getName() + " i : data)\n");
		b.append("{\n");
		b.append(addTreeItems(ROOT_TREE_ITEM, rootItem));
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "List<" + tree.getDTO().getName() + "> fetchRootItems()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#fetchRootItems()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + " throws Exception\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + tree.getDTO().getDomainObject().getLabel() + " items");

		b.append("\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, boundaryBean, b);
		declarationGenerator.addLocalVariable();

		b.append("\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("try\n");
			b.append("{\n");
		}

		final var invocationGenerator = new ServiceInvocationGenerator(tree.getBoundaryMethod(), b);

		b.append("final List<" + rootItem.getItemDTO().getName() + "> items = ");

		if (tree.needsSearchObject())
			invocationGenerator.addInvocation("searchObj");
		else
			invocationGenerator.addInvocation();

		final var compName = rootItem.getItemDTO().getDomainObject().getName() + "Comparator";

		b.append("Collections.sort(items, new " + compName + "());\n");
		b.append("return items;\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "SearchDTO initAdvancedSearch()";

		if (addAdvSearch) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#initAdvancedSearch()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("SearchDTO searchObjAdv;\n\n");
			b.append("if(SearchManager.getLastSearch(getViewID()) == null)\n");
			b.append("{\n");
			b.append("// Initialize search object\n");
			b.append("searchObjAdv = new SearchDTO();\n");
			b.append("searchObjAdv.setDateFormat(userFormat.getDateFormat());\n");
			b.append("searchObjAdv.setDateTimeFormat(userFormat.getDateTimeFormat());\n");
			b.append("searchObjAdv.setNumberFormat(userFormat.getDecimalFormat());\n");
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
				b.append("final var " + searchFieldName + " = new SearchFieldDTO(" + (fieldIndex - 1));
				b.append(", " + a.getDTOAttribute().getSelectTokenConstant());
				b.append(", " + i18n.getI18N(a.getDTOAttribute(), a.getLabel()) + ", " + fieldType + ", 80);\n");

				if (attribute.getJavaType().isLocalDate() || attribute.getTemporalType() == TemporalTypeEnumeration.DATE)
					b.append(searchFieldName + ".setDateTimeFormat(false);\n\n");

				b.append("searchObjAdv.getSearchFields().add(" + searchFieldName + ");\n");

				if (attribute.getJavaType().isEnum()) {
					final var javaEnum = (JavaEnum) attribute.getJavaType();

					// Generate translations for all literals
					javaEnum.getEnumerationValues().forEach(i18n::getI18N);

					b.append("\n");
					b.append("final var enumListValues" + fieldIndex + " = new HashMap<String, String>();\n");
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
			b.append("searchObjAdv = SearchManager.getLastSearch(getViewID());\n\n");
			b.append("return searchObjAdv;\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		methodSignature = "ContextMenu getContextMenuForTreeItem(TreeDataItem item)";

		boolean firstMenu = true;

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#getContextMenuForTreeItem(");
		b.append("net.codecadenza.runtime.richclient.javafx.tree.TreeDataItem)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(item.getGroupName() == null)\n");
		b.append("return null;\n\n");

		for (final Map.Entry<String, String> entry : menuMap.entrySet()) {
			final String groupName = entry.getValue();

			if (firstMenu)
				firstMenu = false;
			else
				b.append("else ");

			b.append("if(item.getGroupName().equals(" + groupName + "))\n");
			b.append("return " + entry.getKey() + ";\n");
		}

		b.append("\nreturn null;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String toString()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.lang.Object#toString()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return " + i18n.getI18N(tree) + ";\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void initialize()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#initialize()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("super.initialize();\n\n");

		// Invoke the context menu initializer methods
		for (final String initMethod : menuMethodInitializerSet)
			b.append(initMethod + "();\n");

		b.append("\n");

		if (addKeyListener())
			b.append("addKeyListener();\n");

		if (hasUpdateOrReadonlyForm())
			b.append("addMouseListener();\n");

		if (!addQuickSearch && !addAdvSearch)
			b.append("\nrefreshView();\n");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method to fetch the next item hierarchy level
	 * @param rootItem
	 */
	private void addRecursiveMethod(TreeViewItem rootItem) {
		final var b = new StringBuilder();
		final String dtoName = rootItem.getItemDTO().getName();
		final var fetchMethodName = "add" + rootItem.getItemDTO().getDomainObject().getNamePlural() + "OfParent"
				+ rootItem.getItemDTO().getDomainObject().getName();
		final var methodSignature = "void " + fetchMethodName + "(TreeDataItem parentItem)";
		final var declarationGenerator = new ServiceDeclarationGenerator(this, tree.getRecursiveMethod(), b);

		b.append("/**\n");
		b.append(" * @param parentItem\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + rootItem.getItemDTO().getDomainObject().getLabel() + " sub-items");

		b.append("\n");

		declarationGenerator.addLocalVariable();

		b.append("\n");
		b.append("if(!(parentItem.getData() instanceof final " + dtoName + " parent))\n");
		b.append("return;\n\n");
		b.append("statusBar.showProgress();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final List<" + dtoName + "> items = ");

		new ServiceInvocationGenerator(tree.getRecursiveMethod(), b)
				.addInvocation("parent." + rootItem.getItemDTO().getPKAttribute().getGetterName());

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
		b.append("final String title = " + i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data!") + ";\n");
		b.append("statusBar.stopProgress();\n\n");
		b.append("DialogUtil.openErrorDialog(null, title, e);\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");

		declarationGenerator.addCloseStatement();

		b.append("statusBar.stopProgress();\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Method for adding the action for creating new objects
	 */
	private void addCreateAction() {
		final DomainObject rootDomainObject = rootItem.getItemDTO().getDomainObject();
		final String actionClassName = "Create" + rootDomainObject.getName() + "Action";
		final Form createForm = getCreateNewForm(rootItem);
		final var actionName = "actionCreate" + rootDomainObject.getName();
		final var methodSignature = "void initActions()";
		var b = new StringBuilder();

		if (createForm == null)
			return;

		importPackage("net.codecadenza.runtime.richclient.javafx.control");
		importPackage("net.codecadenza.runtime.richclient.javafx.dialog");
		importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

		b.append("/**\n");
		b.append(" * Action to create new " + rootDomainObject.getLabel() + "\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private class " + actionClassName + " extends Action\n");
		b.append("{\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public " + actionClassName + "()\n");
		b.append("{\n");
		b.append("this.title = " + i18n.getI18NMessage("action_name_create", "Create") + ";\n");
		b.append("this.image = ImageLoader.getImage(ImageLoader.IMG_NEW_DATA);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void handle()\n");
		b.append("{\n");
		b.append("final var dlg = new " + createForm.getName() + "(null);\n\n");
		b.append("if(DialogButtonType.OK != dlg.open())\n");
		b.append("return;\n\n");
		b.append("refreshView();\n");
		b.append("}\n");
		b.append("}\n\n");

		addSubClass(actionClassName, b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#initActions()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected " + methodSignature + "\n");
		b.append("{\n");
		b.append("super.initActions();\n\n");
		b.append(actionName + " = new " + actionClassName + "();\n");
		b.append("toolBar.getItems().add(" + actionName + ".createToolbarButton());\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the drop listener
	 */
	private void createDropListener() {
		final var methodSignature = "void initDrop()";

		if (dropItems.isEmpty())
			return;

		importPackage("javafx.scene.input");

		final var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#");
		b.append("startDrag(net.codecadenza.runtime.richclient.javafx.tree.TreeDataItem)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public TransferMode startDrag(TreeDataItem item)\n");
		b.append("{\n");

		boolean firstItem = true;

		for (final TreeViewItem item : dropItems) {
			if (firstItem)
				firstItem = false;
			else
				b.append("else ");

			b.append("if(item.getData() instanceof " + item.getItemDTO().getName() + ")\n");

			if (item.isRootItem() || item.getAssociation() instanceof OneToManyAssociation)
				b.append("return TransferMode.MOVE;\n");
			else
				b.append("return TransferMode.COPY;\n");
		}

		firstItem = true;

		b.append("\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.tree.AbstractTreeView#onDragDropped");
		b.append("(javafx.scene.input.DragEvent, net.codecadenza.runtime.richclient.javafx.tree.TreeDataItem)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void onDragDropped(DragEvent dragEvent, TreeDataItem dropItem)\n");
		b.append("{\n");

		for (final TreeViewItem item : dropItems) {
			final String dtoName = item.getItemDTO().getName();
			final String pkAttrGetter = item.getItemDTO().getPKAttribute().getGetterName();
			final String groupName;

			if (item.isRootItem())
				groupName = tree.getDomainObject().getName().toUpperCase() + GROUP_SUFFIX;
			else
				groupName = item.getAssociation().getName().toUpperCase() + GROUP_SUFFIX;

			if (firstItem)
				firstItem = false;
			else
				b.append("else ");

			b.append("if(dropItem.getGroupName() != null && dropItem.getGroupName().equals(" + groupName + ") && ");
			b.append("dragItem.getData() instanceof final " + dtoName + " dragDTO)\n");
			b.append("{\n");

			final var s = new StringBuilder();

			if (!item.isRootItem()) {
				final String pkTypeName = item.getAssociation().getDomainObject().getPKAttribute().getJavaType().getName();

				s.append("final var parentId = (" + pkTypeName + ") dropItem.getData();\n");
			}
			else
				s.append("final var dropDTO = (" + dtoName + ") dropItem.getData();\n");

			final DTOBeanAttribute pkAttr = item.getItemDTO().getPKAttribute();
			final JavaType pkType = pkAttr.getDomainAttribute().getJavaType();
			final BoundaryMethod dropMethod = item.getDropMethod();
			final var declarationGenerator = new ServiceDeclarationGenerator(this, dropMethod, s);

			declarationGenerator.addLocalVariable();

			s.append("\n");
			s.append("try\n");
			s.append("{\n");

			if (item.isRootItem()) {
				s.append("// Avoid cyclic reference!\n");

				if (pkType.isPrimitive())
					s.append("if(dragDTO." + pkAttrGetter + " == dropDTO." + pkAttrGetter + ")\n");
				else
					s.append("if(dragDTO." + pkAttrGetter + ".equals(dropDTO." + pkAttrGetter + "))\n");

				s.append("return;\n\n");

				new ServiceInvocationGenerator(dropMethod, s).addInvocation("dragDTO." + pkAttrGetter, "dropDTO." + pkAttrGetter);

				s.append("}\n");
				s.append("catch (final Exception e)\n");
				s.append("{\n");

				addErrorLog(s, "Drop operation failed!", "e");

				s.append("\n");
				s.append("final String msg = " + i18n.getI18NMessage("msg_err_drop_op", "Drop operation failed!") + ";\n");
				s.append("final String title = " + i18n.getI18NMessage("msg_title_drop_op", "Drop operation") + ";\n\n");
				s.append("DialogUtil.openErrorDialog(null, title, msg, e);\n");
				s.append("return;\n");
				s.append("}\n");

				declarationGenerator.addCloseStatementInFinallyBlock();

				s.append("\n");
				s.append("dragEvent.consume();\n\n");
				s.append("final TreeItem<?> parentDragItem = dragItem.getParent();\n");
				s.append("parentDragItem.getChildren().remove(dragItem);\n\n");
				s.append("dropItem.getChildren().add(dragItem.copy());\n");
			}
			else if (item.getAssociation() instanceof OneToManyAssociation) {
				new ServiceInvocationGenerator(dropMethod, s).addInvocation("dragDTO." + pkAttrGetter, "parentId");

				s.append("}\n");
				s.append("catch (final Exception e)\n");
				s.append("{\n");

				addErrorLog(s, "Drop operation failed!", "e");

				s.append("\n");
				s.append("final String msg = " + i18n.getI18NMessage("msg_err_drop_op", "Drop operation failed!") + ";\n");
				s.append("final String title = " + i18n.getI18NMessage("msg_title_drop_op", "Drop operation") + ";\n\n");
				s.append("DialogUtil.openErrorDialog(null, title, msg, e);\n");
				s.append("return;\n");
				s.append("}\n");

				declarationGenerator.addCloseStatementInFinallyBlock();

				s.append("\n");
				s.append("dragEvent.consume();\n\n");
				s.append("final TreeItem<?> parentDragItem = dragItem.getParent();\n");
				s.append("parentDragItem.getChildren().remove(dragItem);\n\n");
				s.append("dropItem.getChildren().add(dragItem.copy());\n");
			}
			else {
				new ServiceInvocationGenerator(dropMethod, s).addInvocation("parentId", "dragDTO." + pkAttrGetter);

				s.append("\n");
				s.append("// Refresh parent node\n");
				s.append("add" + item.getAssociation().getUpperCaseName() + "(dropItem);\n");
				s.append("}\n");
				s.append("catch (final DuplicateCollectionEntryException e)\n");
				s.append("{\n");
				s.append("final String msg = ");
				s.append(i18n.getI18NMessage("msg_err_duplicate_entry", "Duplicate entry in list is disallowed!") + ";\n");
				s.append("final String title = " + i18n.getI18NMessage("msg_title_drop_op", "Drop operation") + ";\n\n");
				s.append("DialogUtil.openWarningDialog(null, title, msg);\n");
				s.append("}\n");
				s.append("catch (final Exception e)\n");
				s.append("{\n");

				addErrorLog(s, "Drop operation failed!", "e");

				s.append("\n");
				s.append("final String msg = " + i18n.getI18NMessage("msg_err_drop_op", "Drop operation failed!") + ";\n");
				s.append("final String title = " + i18n.getI18NMessage("msg_title_drop_op", "Drop operation") + ";\n\n");
				s.append("DialogUtil.openErrorDialog(null, title, msg, e);\n");
				s.append("}\n");

				declarationGenerator.addCloseStatementInFinallyBlock();
			}

			if (addSecurity && dropMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(dropMethod.getRoles(), s.toString()));
			else
				b.append(s);

			b.append("}\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the key listener method
	 */
	private void createKeyListener() {
		final var b = new StringBuilder();
		final var methodSignature = "void addKeyListener()";
		final boolean addEnterHandler = hasUpdateOrReadonlyForm();

		importPackage("javafx.scene.input");

		b.append("/**\n");
		b.append(" * Add key listener to tree view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("treeView.setOnKeyPressed(e ->\n");
		b.append("{\n");
		b.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
		b.append("return;\n\n");
		b.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
		b.append("if(selectedItem.getData() == null)\n");
		b.append("return;\n\n");

		if (addEnterHandler)
			b.append(addEnterKeyHandler());

		if (hasDeleteMethod())
			b.append(addDeleteKeyHandler(addEnterHandler));

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

		b.append("if(e.getCode().equals(KeyCode.ENTER))\n");
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

				b.append("(selectedItem.getData() instanceof final " + dto.getName() + " dto)\n");
				b.append("{\n");

				var methodName = "";

				if (editForm != null) {
					methodName = "edit" + dto.getDomainObject().getName();
					final var openEdit = methodName + "(dto);\nreturn;\n";

					b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), openEdit));
				}

				if (addSecurity && viewForm != null) {
					methodName = "view" + dto.getDomainObject().getName();
					final var openView = methodName + "(dto);\n";

					if (editForm != null)
						b.append("\n");

					b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), openView));
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

		b.append("if(e.getCode().equals(KeyCode.DELETE))\n");
		b.append("{\n");

		// Add the delete method for the root tree item
		final BoundaryMethod deleteMethodOfRootItem = getDeleteMethod(rootItem);
		boolean firstIf = true;

		if (deleteMethodOfRootItem != null) {
			final var methodName = "delete" + rootItem.getItemDTO().getDomainObject().getName();
			firstIf = false;

			b.append("if(selectedItem.getData() instanceof final " + rootItem.getItemDTO().getName() + " dto)\n");
			b.append("{\n");

			final var s = new StringBuilder();
			s.append("final var parentItem = (TreeDataItem) selectedItem.getParent();\n");
			s.append("final boolean success = " + methodName + "(dto);\n\n");
			s.append("if(success)\n");
			s.append("{\n");
			s.append("parentItem.getChildren().remove(selectedItem);\n");
			s.append("return;\n");
			s.append("}\n");

			if (addSecurity && deleteMethodOfRootItem.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(deleteMethodOfRootItem.getRoles(), s.toString()));
			else
				b.append(s.toString());

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
				final String parentGroupName = entry.getKey().getAssociation().getName().toUpperCase() + GROUP_SUFFIX;
				final JavaType pkRefType = entry.getKey().getAssociation().getDomainObject().getPKAttribute().getJavaType();

				if (firstIf)
					firstIf = false;
				else
					b.append("else ");

				b.append("if(selectedItem.getData() instanceof final " + dto.getName() + " item");
				b.append(" && ((TreeDataItem) selectedItem.getParent()).getGroupName().equals(" + parentGroupName + "))\n");
				b.append("{\n");

				final var s = new StringBuilder();
				s.append("final var parentItem = (TreeDataItem) selectedItem.getParent();\n\n");
				s.append("if(!(parentItem.getData() instanceof final " + pkRefType.getWrapperTypeName() + " parentId))\n");
				s.append("return;\n\n");
				s.append("final boolean success = " + removeMethod.getName() + "(parentId, item);\n\n");
				s.append("if(success)\n");
				s.append("{\n");
				s.append("parentItem.getChildren().remove(selectedItem);\n");
				s.append("return;\n");
				s.append("}\n");

				if (addSecurity && removeMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
					b.append(securityHelper.wrapSecurityCode(removeMethod.getRoles(), s.toString()));
				else
					b.append(s.toString());

				b.append("}\n");
			}

			if (deleteMethod != null) {
				final var methodName = "delete" + dto.getDomainObject().getName();

				if (firstIf)
					firstIf = false;
				else
					b.append("else ");

				b.append("if(selectedItem.getData() instanceof final " + dto.getName() + " dto)\n");
				b.append("{\n");

				final var s = new StringBuilder();
				s.append("final var parentItem = (TreeDataItem) selectedItem.getParent();\n");
				s.append("final boolean success = " + methodName + "(dto);\n\n");
				s.append("if(success)\n");
				s.append("{\n");
				s.append("parentItem.getChildren().remove(selectedItem);\n");
				s.append("return;\n");
				s.append("}\n");

				if (addSecurity && deleteMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
					b.append(securityHelper.wrapSecurityCode(deleteMethod.getRoles(), s.toString()));
				else
					b.append(s.toString());

				b.append("}\n");
			}
		}

		b.append("}\n");

		return b.toString();
	}

	/**
	 * Add the mouse listener method
	 */
	private void createMouseListener() {
		final var b = new StringBuilder();
		final var methodSignature = "void addMouseListener()";
		boolean isFirstDTO = true;

		b.append("/**\n");
		b.append(" * Add mouse and menu listener to tree view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("treeView.setOnMouseClicked(e ->\n");
		b.append("{\n");
		b.append("if(e.getClickCount() != 2)\n");
		b.append("return;\n\n");
		b.append("if(treeView.getSelectionModel().getSelectedItem() == null)\n");
		b.append("return;\n\n");
		b.append("final var selectedItem = (TreeDataItem) treeView.getSelectionModel().getSelectedItem();\n\n");
		b.append("if(selectedItem.getData() == null)\n");
		b.append("return;\n\n");

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

				b.append("(selectedItem.getData() instanceof final " + dto.getName() + " dto)\n");
				b.append("{\n");

				var methodName = "";

				if (editForm != null) {
					methodName = "edit" + dto.getDomainObject().getName();

					b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\nreturn;\n"));
				}

				if (addSecurity && viewForm != null) {
					methodName = "view" + dto.getDomainObject().getName();

					if (editForm != null)
						b.append("\n");

					b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), methodName + "(dto);\n"));
				}

				b.append("}\n");
			}
		}

		b.append("});\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
