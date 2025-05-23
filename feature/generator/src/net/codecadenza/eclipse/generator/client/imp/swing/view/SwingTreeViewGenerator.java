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
package net.codecadenza.eclipse.generator.client.imp.swing.view;

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
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;

import java.util.Collection;
import java.util.Map;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.common.tree.AbstractTreeViewGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.file.SwingFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingSecurityHelper;
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
 * Generator for tree views of a Swing application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingTreeViewGenerator extends AbstractTreeViewGenerator {
	private static final String FIELD_NAME_TREE = "tree";

	private final RichClientI18NGenerator i18n;
	private final SwingSecurityHelper securityHelper;

	/**
	 * Constructor
	 * @param tree
	 */
	public SwingTreeViewGenerator(TreeView tree) {
		super(tree);

		this.i18n = new RichClientI18NGenerator(project);
		this.securityHelper = new SwingSecurityHelper(project);
		this.addSecurity = securityHelper.isSecurityAdded();

		initializeInternalFields();

		// The generator methods must be called in order to initialize field 'menuMethodInitializerSet'!
		addTreeMenuMethods();
		addAllSubItemMenuMethods(rootItem.getChildren());

		// Remove all content that has been created during the initialization!
		clearContent();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage("java.util");
		importPackage("java.util.concurrent");
		importPackage("java.awt");
		importPackage("java.awt.event");
		importPackage("javax.swing");
		importPackage("javax.swing.event");
		importPackage("net.codecadenza.runtime.richclient.swing.image");
		importPackage("net.codecadenza.runtime.richclient.swing.widget");

		if (!dropItems.isEmpty()) {
			importPackage("java.awt.dnd");
			importPackage("javax.swing.tree");
			importPackage("java.awt.datatransfer");
		}

		addImports(securityHelper.getSecurityImports());

		if (tree.needsSearchObject())
			importPackage("net.codecadenza.runtime.search.dto");

		if (addAdvSearch) {
			importPackage("net.codecadenza.runtime.richclient.swing.dialog");
			importPackage("net.codecadenza.runtime.richclient.swing.search.input");
			importPackage("net.codecadenza.runtime.richclient.swing.search");
			importPackage("net.codecadenza.runtime.richclient.search.util");
		}

		if (needsDateFormatter || needsDateTimeFormatter)
			importPackage(PACK_JAVA_TIME_FORMAT);

		if (needsDecimalFormatter)
			importPackage("java.text");

		// Add all DTO imports
		importPackage(rootItem.getItemDTO().getNamespace().toString());

		addSubItemDTOImports(rootItem.getChildren());

		addSubItemBoundaryImports(rootItem.getChildren());

		// Check if imports to download LOB attributes are necessary
		if (!getDownloadMethods(rootItem).isEmpty()) {
			if (project.isJavaSEApplication()) {
				importClass("java.io.File");
				importPackage("net.codecadenza.runtime.file");
			}
			else
				importPackage("net.codecadenza.runtime.richclient.swing.file");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + tree.getName() + " extends JInternalFrame implements StatusReceivable");

		if (addAdvSearch)
			b.append(", Countable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addProtectedConstant(JavaType.STRING, "ITEM_LABEL_SEPARATOR", "\": \"").create();
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPrivateField("JDataTree", FIELD_NAME_TREE).withFinalModifier().create();
		addPrivateField("DataTreeNode", "rootNode").create();
		addPrivateField("JStatusPanel", "statusPanel").withFinalModifier().create();
		addPrivateField(JavaType.INT, "counter").create();
		addPrivateField("RefreshAction", "actionRefresh").create();

		boolean addFormat = false;

		if (tree.needsSearchObject())
			addFormat = true;

		if (needsDateFormatter || needsDateTimeFormatter || needsDecimalFormatter)
			addFormat = true;

		if (addFormat) {
			addPrivateField("FormatDTO", "format").withDefaultValue("FormatPreferencesManager.getFormatDTO()").withFinalModifier()
					.create();

			importPackage("net.codecadenza.runtime.richclient.format");
		}

		if (needsDateTimeFormatter) {
			final var formatter = "DateTimeFormatter.ofPattern(format.getDateTimeFormat()).withZone(" + PACK_JAVA_TIME
					+ ".ZoneId.systemDefault())";

			addPrivateField("DateTimeFormatter", "dateTimeFormat").withDefaultValue(formatter).withTransientModifier()
					.withFinalModifier().create();
		}

		if (needsDateFormatter) {
			final var formatter = "DateTimeFormatter.ofPattern(format.getDateFormat()).withZone(" + PACK_JAVA_TIME
					+ ".ZoneId.systemDefault())";

			addPrivateField("DateTimeFormatter", "dateFormat").withDefaultValue(formatter).withTransientModifier().withFinalModifier()
					.create();
		}

		if (needsDecimalFormatter)
			addPrivateField("DecimalFormat", "decimalFormat").withDefaultValue("new DecimalFormat(format.getDecimalFormat())")
					.withFinalModifier().create();

		final DomainObject rootDomainObject = rootItem.getItemDTO().getDomainObject();
		final var menuName = "menu" + rootDomainObject.getName();

		addPrivateField("JPopupMenu", menuName).create();

		menuSet.add(menuName);

		// Add all context menu declarations for the sub-items
		addMenuFields(rootItem.getChildren());

		// Add the action for creating new objects
		if (getCreateNewForm(rootItem) != null) {
			final var actionName = "actionCreate" + rootDomainObject.getName();

			addPrivateField("Create" + rootDomainObject.getName() + "Action", actionName).create();
		}

		if (addAdvSearch)
			addPrivateField("AdvancedSearchAction", "actionAdvSearch").create();

		// Add fields for performing a quick-search!
		if (addQuickSearch) {
			tree.getQuickSearchItems().forEach(a -> {
				final DTOBeanAttribute attr = a.getDTOAttribute();
				final var fieldName = "txt" + attr.getUpperCaseName();

				addPrivateField("JTextField", fieldName).withFinalModifier().create();
			});
		}

		// Add a search object if necessary
		if (tree.needsSearchObject())
			addPrivateField("SearchDTO", "searchObj").create();

		addPrivateField("SwingWorker<Void, Void>", "dataFetchThread").withTransientModifier().create();
		addPrivateField("StopSearchAction", "actionStopSearch").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		final var methodSignature = tree.getName() + "()";

		String title = rootItem.getItemDTO().getDomainObject().getLabel();
		title = title.substring(0, 1).toUpperCase() + title.substring(1) + " tree view";

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Initialize tree view");

		b.append("\n");
		b.append("setResizable(true);\n");
		b.append("setClosable(true);\n");
		b.append("setTitle(" + i18n.getI18NMessage(tree.getName(), title) + ");\n");
		b.append("setLayout(new BorderLayout(0, 0));\n\n");

		// Add an action listener to close the view if the user presses ESC
		b.append("final ActionListener actionListener = actionEvent ->\n");
		b.append("{\n");
		b.append(tree.getName() + ".this.getDesktopPane().getDesktopManager().closeFrame(" + tree.getName() + ".this);\n");
		b.append("dispose();\n");
		b.append("};\n\n");
		b.append("final KeyStroke stroke = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);\n");
		b.append("rootPane.registerKeyboardAction(actionListener, stroke, JComponent.WHEN_IN_FOCUSED_WINDOW);\n\n");
		b.append("final var panFilter = new JPanel();\n\n");
		b.append("final var gblFilter = new GridBagLayout();\n");
		b.append("gblFilter.columnWidths = new int[] { 0, 0, 0 };\n");
		b.append("gblFilter.columnWeights = new double[] {0, 1.0, Double.MIN_VALUE };\n");

		int rowCounter = 0;

		if (addQuickSearch)
			rowCounter = tree.getQuickSearchItems().size();

		var rowHeights = "gblFilter.rowHeights = new int[] {0";
		var rowWeights = "gblFilter.rowWeights = new double[] {0";

		for (int i = 0; i < rowCounter; i++) {
			rowHeights += ",0";
			rowWeights += ",1.0";
		}

		rowHeights += "};\n";
		rowWeights += "};\n";

		b.append(rowHeights);
		b.append(rowWeights);
		b.append("\n");
		b.append("panFilter.setLayout(gblFilter);\n\n");
		b.append("createActions();\n\n");
		b.append("final var toolBar = new JToolBar();\n");
		b.append("toolBar.setFloatable(false);\n");
		b.append("toolBar.setRollover(true);\n");
		b.append("toolBar.add(actionRefresh);\n");

		if (addAdvSearch)
			b.append("toolBar.add(actionAdvSearch);\n");

		if (tree.needsSearchObject())
			b.append("toolBar.add(actionStopSearch);\n");

		final Form createForm = getCreateNewForm(rootItem);

		if (createForm != null) {
			final var actionName = "actionCreate" + rootItem.getItemDTO().getDomainObject().getName();
			final var toolBarItemCreate = "toolBar.add(" + actionName + ");\n";

			if (addSecurity)
				b.append("\n");

			b.append(securityHelper.wrapSecurityCode(createForm.getRoles(), toolBarItemCreate));

			if (addSecurity)
				b.append("\n");
		}

		b.append("\n");
		b.append("final var gbcToolBar = new GridBagConstraints();\n");
		b.append("gbcToolBar.insets = new Insets(0, 0, 0, 0);\n");
		b.append("gbcToolBar.fill = GridBagConstraints.HORIZONTAL;\n");
		b.append("gbcToolBar.gridx = 0;\n");
		b.append("gbcToolBar.gridy = 0;\n");
		b.append("gbcToolBar.gridwidth = 2;\n\n");
		b.append("panFilter.add(toolBar, gbcToolBar);\n\n");

		if (addQuickSearch) {
			int rowIndex = 1;

			for (final TreeSearchItem s : tree.getQuickSearchItems()) {
				final var labelName = "lbl" + s.getDTOAttribute().getUpperCaseName();
				final var labelConstrName = "gbcLbl" + s.getDTOAttribute().getUpperCaseName();
				final var txtName = "txt" + s.getDTOAttribute().getUpperCaseName();
				final var txtConstrName = "gbcTxt" + s.getDTOAttribute().getUpperCaseName();

				b.append("final var " + labelName + " = new JLabel(" + i18n.getI18N(s.getDTOAttribute(), s.getLabel(), true) + ");\n\n");
				b.append("final var " + labelConstrName + " = new GridBagConstraints();\n");
				b.append(labelConstrName + ".anchor = GridBagConstraints.BASELINE_LEADING;\n");
				b.append(labelConstrName + ".insets = new Insets(5, 5, 5, 5);\n");
				b.append(labelConstrName + ".gridx = 0;\n");
				b.append(labelConstrName + ".gridy = " + rowIndex + ";\n\n");
				b.append("panFilter.add(" + labelName + ", " + labelConstrName + ");\n\n");
				b.append(txtName + " = new JTextField();\n\n");
				b.append("final var " + txtConstrName + " = new GridBagConstraints();\n");
				b.append(txtConstrName + ".fill = GridBagConstraints.HORIZONTAL;\n");
				b.append(txtConstrName + ".anchor = GridBagConstraints.BASELINE_LEADING;\n");
				b.append(txtConstrName + ".insets = new Insets(5, 5, 5, 5);\n");
				b.append(txtConstrName + ".gridx = 1;\n");
				b.append(txtConstrName + ".gridy = " + rowIndex + ";\n\n");
				b.append("panFilter.add(" + txtName + ", " + txtConstrName + ");\n\n");
				b.append(txtName + ".addKeyListener(new KeyAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void keyPressed(final KeyEvent e)\n");
				b.append("{\n");
				b.append("if(e.getKeyCode() == KeyEvent.VK_ENTER)\n");
				b.append("performQuickSearch();\n");
				b.append("}\n");
				b.append("});\n\n");

				rowIndex++;
			}
		}

		if (tree.needsSearchObject()) {
			b.append("// Initialize search object\n");
			b.append("searchObj = new SearchDTO();\n");
			b.append("searchObj.setDateFormat(format.getDateFormat());\n");
			b.append("searchObj.setDateTimeFormat(format.getDateTimeFormat());\n");
			b.append("searchObj.setNumberFormat(format.getDecimalFormat());\n");
			b.append("searchObj.setCaseSensitive(true);\n");
			b.append("searchObj.setExactFilterMatch(true);\n");
			b.append("searchObj.setCount(false);\n");
			b.append("searchObj.setMaxResult(1000);\n\n");
		}

		final var msgKey = tree.getName() + "_root_item";
		String rootLabel = rootItem.getItemDTO().getDomainObject().getLabel();
		rootLabel = rootLabel.substring(0, 1).toUpperCase() + rootLabel.substring(1);

		// Create the root node of the tree
		b.append("rootNode = new DataTreeNode(" + i18n.getI18NMessage(msgKey, rootLabel));
		b.append(", ImageLoader.getImage(ImageLoader.FOLDER));\n");
		b.append(FIELD_NAME_TREE + " = new JDataTree(rootNode);\n");

		if (!dropItems.isEmpty())
			b.append(FIELD_NAME_TREE + ".setDropMode(DropMode.ON);\n");

		b.append("\nfinal var scrollPane = new JScrollPane();\n");
		b.append("scrollPane.setViewportView(" + FIELD_NAME_TREE + ");\n\n");
		b.append("statusPanel = new JStatusPanel();\n\n");
		b.append("add(panFilter, BorderLayout.NORTH);\n");
		b.append("add(scrollPane, BorderLayout.CENTER);\n");
		b.append("add(statusPanel, BorderLayout.SOUTH);\n\n");

		if (!dropItems.isEmpty())
			b.append("initDrop();\n");

		// Invoke the context menu initializer methods
		menuMethodInitializerSet.forEach(initMethod -> b.append(initMethod + "();\n"));

		// Remove all elements from the sets. Otherwise, some methods won't be added in subsequent generator steps!
		menuMethodInitializerSet.clear();
		methodSet.clear();

		b.append("\n");

		if (addKeyListener())
			b.append("addKeyListener();\n");

		b.append("addMouseListener();\n");
		b.append("addTreeListener();\n\n");

		if (!addAdvSearch && !addQuickSearch)
			b.append("buildTree();\n\n");

		addDebugLog(b, "Tree view initialization finished");

		b.append("}\n\n");

		addConstructor(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		// Add helper DTOs
		addHelperDTOs();

		// Add comparators
		addComparatorClasses(rootItem);

		// Add the method to perform a quick-search
		if (addQuickSearch)
			addQuickSearchMethod();

		// Add the count method
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

		// Add the method to build the tree
		addTreeBuildMethod(tree.getName());

		// Add actions
		addActions();

		// Add the key listener method
		if (addKeyListener())
			createKeyListener();

		// Add the drop target listener method
		createDropListener();

		// Add the tree listener
		createTreeListener(rootItem);

		// Add the mouse listener
		createMouseListener();

		// Add the methods to implement interface StatusReceivable
		addStatusImpMethods();

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
			// Add imports for download operations
			if (!getDownloadMethods(item).isEmpty()) {
				if (project.isJavaSEApplication()) {
					importClass("java.io.File");
					importClass("java.awt.Cursor");
					importPackage("net.codecadenza.runtime.file");
				}
				else
					importPackage("net.codecadenza.runtime.richclient.swing.file");
			}

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
				addPrivateField("JPopupMenu", menuName).create();

				menuSet.add(menuName);
			}

			// Add the context menu for the parent of this item
			final String assocName = item.getAssociation().getUpperCaseName();

			menuName = "menu" + assocName;

			addPrivateField("JPopupMenu", menuName).create();

			addMenuFields(item.getChildren());
		}
	}

	/**
	 * Create private helper data transfer objects
	 */
	private void addHelperDTOs() {
		if (tree.getRecursiveMethod() != null) {
			final DomainObject rootDomainObject = rootItem.getItemDTO().getDomainObject();
			final var dtoName = rootDomainObject.getName() + "TreeHelperDTO";
			final String pkTypeName = rootDomainObject.getPKAttribute().getJavaType().getName();

			final var b = new StringBuilder();
			b.append("\n");
			b.append("/**\n");
			b.append(" * Helper class for tree view sub-items\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private class " + dtoName + " extends AbstractSubItemHelperDTO\n");
			b.append("{\n");
			b.append("private static final long serialVersionUID = 1L;\n");
			b.append("private final " + pkTypeName + " parentId;\n\n");
			b.append("public " + dtoName + "(" + pkTypeName + " parentId)\n");
			b.append("{\n");
			b.append("this.parentId = parentId;\n");
			b.append("}\n\n");
			b.append("public " + pkTypeName + " getParentId()\n");
			b.append("{\n");
			b.append("return parentId;\n");
			b.append("}\n");
			b.append("}\n\n");

			addSubClass(dtoName, b.toString());
		}

		addSubItemHelperDTOs(rootItem.getChildren());
	}

	/**
	 * Add the count method
	 */
	private void addCountMethod() {
		final var b = new StringBuilder();
		final BoundaryMethod countMethod = tree.getCountMethod();
		final var methodSignature = "long countData(SearchDTO searchDTO)";
		final var declarationGenerator = new ServiceDeclarationGenerator(this, countMethod, b);

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.search.Countable#");
		b.append("countData(net.codecadenza.runtime.search.dto.SearchDTO)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		declarationGenerator.addLocalVariable();

		b.append("\n");
		b.append("try\n");
		b.append("{\n");
		b.append("return ");

		new ServiceInvocationGenerator(countMethod, b).addInvocation("searchDTO");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while performing count operation!", "e");

		b.append("\n");
		b.append("JOptionPane.showMessageDialog(this, ");
		b.append(i18n.getI18NMessage("msg_err_count", "Error while counting records! Message: "));
		b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_count", "Count records"));
		b.append(", JOptionPane.ERROR_MESSAGE);\n");
		b.append("}\n");

		declarationGenerator.addCloseStatementInFinallyBlock();

		b.append("\n");
		b.append("return 0;\n");
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
			b.append("final var " + searchFieldName + " = new SearchFieldDTO(" + (fieldCount - 1));
			b.append(", " + a.getDTOAttribute().getSelectTokenConstant());
			b.append(", " + i18n.getI18N(a.getDTOAttribute(), a.getLabel()) + ", SearchFieldDataTypeEnum.STRING, 80);\n");
			b.append(searchFieldName + ".setFilterCriteria(" + fieldName + ".getText());\n\n");
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
		BoundaryMethod deleteMethod = null;
		StringBuilder b;

		// We have to check if we delete an object or if we remove an object from an association list!
		final BoundaryMethod removeMethod = getSubItemRemoveMethod(treeItem);

		if (removeMethod != null) {
			final String refPKGetter = treeItem.getItemDTO().getPKAttribute().getGetterName();
			final var parentDTOName = treeItem.getAssociation().getUpperCaseName() + "TreeHelperDTO";
			final var methodSignature = "boolean " + removeMethod.getName() + "(" + parentDTOName + " parent, "
					+ treeItem.getItemDTO().getName() + " item)";
			final String assocName = treeItem.getAssociation().getName();

			if (!methodSet.contains(methodSignature)) {
				methodSet.add(methodSignature);

				b = new StringBuilder();
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

				final var declarationGenerator = new ServiceDeclarationGenerator(this, removeMethod, b);
				declarationGenerator.addLocalVariable();

				b.append("\n");
				b.append("try\n");
				b.append("{\n");
				b.append("final int result = JOptionPane.showConfirmDialog(this, ");
				b.append(i18n.getI18NMessage("msg_confirm_remove_item", "Do you really want to remove selected object from list?"));
				b.append(", " + i18n.getI18NMessage("msg_title_remove_item", "Remove item") + ", JOptionPane.YES_NO_OPTION);\n\n");
				b.append("if(result == JOptionPane.NO_OPTION)\n");
				b.append("return false;\n\n");

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
				b.append("JOptionPane.showMessageDialog(this, ");
				b.append(i18n.getI18NMessage("msg_err_remove_item", "Could not remove selected item from list! Message: "));
				b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_remove_item", "Remove item"));
				b.append(", JOptionPane.ERROR_MESSAGE);\n");
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
				b.append("final int result = JOptionPane.showConfirmDialog(this, ");
				b.append(i18n.getI18NMessage("msg_confirm_delete", "Do you really want to delete selected object?"));
				b.append(", " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ", JOptionPane.YES_NO_OPTION);\n\n");
				b.append("if(result == JOptionPane.NO_OPTION)\n");
				b.append("return false;\n\n");

				addDebugLog(b, "Delete selected object with id '{}'", "dto." + pkGetter);

				b.append("\n");

				new ServiceInvocationGenerator(deleteMethod, b).addInvocation("dto." + pkGetter);

				b.append("return true;\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while deleting selected object!", "e");

				b.append("\n");
				b.append("JOptionPane.showMessageDialog(this, ");
				b.append(i18n.getI18NMessage("msg_err_delete", "Could not delete selected object! Message: "));
				b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_delete", "Delete object"));
				b.append(", JOptionPane.ERROR_MESSAGE);\n");
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
			importPackage("net.codecadenza.runtime.richclient.swing.dialog");

			final var methodSignature = "boolean add" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " dto)";

			if (!methodSet.contains(methodSignature)) {
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
				b.append("final var dlg = new " + addForm.getName() + "(" + tree.getName() + ".this, dto." + pkGetter + ");\n");
				b.append("dlg.setVisible(true);\n\n");
				b.append("return dlg.getReturnCode() == JTitleAreaDialog.RETURN_CODE_OK;\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		// Check if a form of type 'UPDATE' exists
		final Form editForm = getEditForm(treeItem);

		if (editForm != null) {
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
			importPackage("net.codecadenza.runtime.richclient.swing.dialog");

			final var methodSignature = "boolean edit" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " dto)";

			if (!methodSet.contains(methodSignature)) {
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
				b.append("final var dlg = new " + editForm.getName() + "(" + tree.getName() + ".this, dto." + pkGetter + ");\n");
				b.append("dlg.setVisible(true);\n\n");
				b.append("return dlg.getReturnCode() == JTitleAreaDialog.RETURN_CODE_OK;\n");
				b.append("}\n\n");

				addMethod(methodSignature, b.toString());
			}
		}

		// Check if a form of type 'READONLY' exists
		final Form viewForm = getReadOnlyForm(treeItem);

		if (viewForm != null) {
			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
			importPackage("net.codecadenza.runtime.richclient.swing.dialog");

			final var methodSignature = "void view" + itemDomainObject.getName() + "(" + treeItem.getItemDTO().getName() + " dto)";

			if (!methodSet.contains(methodSignature)) {
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
				b.append("new " + viewForm.getName() + "(" + tree.getName() + ".this, dto." + pkGetter + ").setVisible(true);\n");
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

				final String typeName = treeItem.getItemDTO().getPKAttribute().getDomainAttribute().getJavaType().getName();
				final String getter = treeItem.getItemDTO().getPKAttribute().getGetterName();

				b.append("final " + typeName + " id = dto." + getter + ";\n");
				b.append(new SwingFileHandlingGenerator(this, downloadMethod, i18n).createDownloadMethodBody(true, tree.getName()));
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
			final var b = new StringBuilder();
			final String assocName = item.getAssociation().getUpperCaseName();
			final var dtoName = assocName + "TreeHelperDTO";
			final var menuMethodName = "initialize" + assocName + "Menu";
			final var methodSignature = "void " + menuMethodName + "()";

			menuMethodInitializerSet.add(menuMethodName);

			b.append("/**\n");
			b.append(" * Initialize menu\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private " + methodSignature + "\n");
			b.append("{\n");
			b.append("menu" + assocName + " = new JPopupMenu();\n\n");
			b.append("// Add menu item to refresh sub-items\n");
			b.append("final var itemRefresh = new JMenuItem(" + i18n.getI18NMessage("action_name_refresh", "Refresh"));
			b.append(", ImageLoader.getImage(ImageLoader.REFRESH));\n");
			b.append("itemRefresh.addActionListener(actionEvent ->\n");
			b.append("{\n");
			b.append("final DataTreeNode selItem = tree.getSelectedTreeNode();\n\n");
			b.append("if(selItem == null)\n");
			b.append("return;\n\n");
			b.append("final var dto = (" + dtoName + ") selItem.getHelperObject();\n");
			b.append("add" + assocName + "(selItem, dto);\n");
			b.append("});\n\n");
			b.append("menu" + assocName + ".add(itemRefresh);\n\n");

			// Check if a form of type 'ADD' exists
			if (item.getAssociation() instanceof OneToManyAssociation) {
				final Form addForm = getAddForm(item);

				if (addForm != null) {
					final var fb = new StringBuilder();
					fb.append("// Add menu item to add " + item.getItemDTO().getDomainObject().getLabel() + "\n");
					fb.append("final var itemAdd = new JMenuItem(" + i18n.getI18NMessage("action_name_add", "Add"));
					fb.append(", ImageLoader.getImage(ImageLoader.NEW_DATA));\n");
					fb.append("itemAdd.addActionListener(actionEvent ->\n");
					fb.append("{\n");
					fb.append("final DataTreeNode selItem = tree.getSelectedTreeNode();\n\n");
					fb.append("if(selItem == null)\n");
					fb.append("return;\n\n");

					final var methodName = "add" + item.getItemDTO().getDomainObject().getName() + "To" + assocName;

					fb.append("final var dto = (" + dtoName + ") selItem.getHelperObject();\n\n");
					fb.append("if(" + methodName + "(dto) && dto.isDataLoaded())\n");
					fb.append("add" + assocName + "(selItem, dto);\n");
					fb.append("});\n\n");
					fb.append("menu" + assocName + ".add(itemAdd);\n");

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
		final var itemName = "item" + item.getItemDTO().getDomainObject().getName();
		boolean isFirstAttribute = true;
		boolean isSecondAttribute = false;

		final var b = new StringBuilder();
		b.append("itemText.setLength(0);\n");

		for (final DTOBeanAttribute attr : item.getDisplayAttributes()) {
			final String getter = "i." + attr.getGetterName();
			final boolean addNullCheck = !attr.getSearchType().isPrimitive();

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

			if (addNullCheck)
				b.append("\nif(" + getter + " != null)\n");

			b.append("itemText.append(" + attr.getDomainAttribute().convertToString(getter) + ");\n");

			if (addNullCheck)
				b.append("\n");
		}

		if (item.getDisplayAttributes().size() > 1)
			b.append("itemText.append(\")\");\n");

		b.append("\nfinal var " + itemName + " = new DataTreeNode(itemText.toString(), ");

		if (item.isRootItem())
			b.append("ImageLoader.getImage(ImageLoader.FOLDER));\n");
		else
			b.append("ImageLoader.getImage(ImageLoader.TREE_DATA));\n");

		b.append(itemName + ".setUserObject(i);\n\n");

		if (FIELD_NAME_TREE.equals(parentItemName))
			b.append(FIELD_NAME_TREE + ".addNodeToParent(" + itemName + ", rootNode);\n");
		else
			b.append(FIELD_NAME_TREE + ".addNodeToParent(" + itemName + ", " + parentItemName + ");\n");

		// Add the item including the helper DTO only for recursive structures
		if (item.isRootItem() && tree.getRecursiveMethod() != null) {
			final String domainObjectName = item.getItemDTO().getDomainObject().getName();
			final var dtoName = domainObjectName + "TreeHelperDTO";
			final var dtoPropertyName = domainObjectName.substring(0, 1).toLowerCase() + domainObjectName.substring(1)
					+ "TreeHelperDTO";
			final String pkGetter = item.getItemDTO().getPKAttribute().getGetterName();

			b.append("\nfinal var " + dtoPropertyName + " = new " + dtoName + "(i." + pkGetter + ");\n");
			b.append(itemName + ".setHelperObject(" + dtoPropertyName + ");\n\n");

			if (item.getNodes().isEmpty() && item.getChildren().isEmpty()) {
				b.append("\n// Add dummy item!\n");
				b.append("final var itemDummy = new DataTreeNode(\"\", null);\n");
				b.append("itemDummy.setUserObject(new " + item.getItemDTO().getName() + "(");
				b.append(item.getItemDTO().getPKAttribute().getDomainAttribute().getEmptyItemDefaultValue());
				b.append("));\n\n");
				b.append(FIELD_NAME_TREE + ".addNodeToParent(itemDummy, " + itemName + ");\n\n");
			}
		}

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

				b.append("\n");
				b.append("final var " + parentNodeName + " = new DataTreeNode(" + i18n.getI18N(node.getDTOAttribute(), node.getLabel()));
				b.append(", ImageLoader.getImage(ImageLoader.FOLDER));\n\n");
				b.append("for(final var element : " + getter + ")\n");
				b.append("{\n");
				b.append("final var " + nodeName + " = new DataTreeNode(");
				b.append(node.getDTOAttribute().getDomainAttribute().convertToString("element"));
				b.append(", ImageLoader.getImage(ImageLoader.TREE_DATA));\n");
				b.append(FIELD_NAME_TREE + ".addNodeToParent(" + nodeName + ", " + parentNodeName + ");\n");
				b.append("}\n");

				nodeName = parentNodeName;
			}
			else if (type.isBoolean()) {
				b.append("\n");
				b.append("final var " + nodeName + " = new DataTreeNode(");
				b.append(i18n.getI18N(node.getDTOAttribute(), node.getLabel()) + ", null);\n\n");
				b.append("if(" + getter + ")\n");
				b.append(nodeName + ".setIcon(ImageLoader.getImage(ImageLoader.CHECKED));\n");
				b.append("else\n");
				b.append(nodeName + ".setIcon(ImageLoader.getImage(ImageLoader.UNCHECKED));\n");
				b.append("\n");
			}
			else {
				if (type instanceof final JavaEnum javaEnum)
					javaEnum.getEnumerationValues().forEach(i18n::getI18N);

				b.append("\n");
				b.append("final var " + nodeName + " = new DataTreeNode(" + i18n.getI18N(node.getDTOAttribute(), node.getLabel()));
				b.append(" + ITEM_LABEL_SEPARATOR + ");
				b.append(node.getDTOAttribute().getDomainAttribute().convertToString(getter));

				if (type.isTemporalType())
					b.append(", ImageLoader.getImage(ImageLoader.CALENDAR));\n");
				else
					b.append(", ImageLoader.getImage(ImageLoader.TREE_DATA));\n");
			}

			b.append("\n");
			b.append(FIELD_NAME_TREE + ".addNodeToParent(" + nodeName + ", " + itemName + ");\n");

			if (addNullCheck)
				b.append("}\n");
		});

		// Add parent items for all sub-items
		item.getChildren().forEach(treeItem -> {
			final var helperDtoClassName = treeItem.getAssociation().getUpperCaseName() + "TreeHelperDTO";
			final var helperDtoPropertyName = treeItem.getAssociation().getName() + "TreeHelperDTO";
			final var subParentItemName = "item" + treeItem.getAssociation().getUpperCaseName();
			final var dummyItemName = treeItem.getAssociation().getName() + "DummyItems";
			final var msgKey = tree.getName() + "_" + treeItem.getAssociation().getName();

			b.append("\n");
			b.append("final var " + helperDtoPropertyName + " = new " + helperDtoClassName + "(");
			b.append("i." + item.getItemDTO().getPKAttribute().getGetterName() + ");\n\n");
			b.append("final var " + subParentItemName + " = new DataTreeNode(" + i18n.getI18NMessage(msgKey, treeItem.getLabel()));
			b.append(", ImageLoader.getImage(ImageLoader.FOLDER));\n");
			b.append(subParentItemName + ".setHelperObject(" + helperDtoPropertyName + ");\n\n");
			b.append(FIELD_NAME_TREE + ".addNodeToParent(" + subParentItemName + ", " + itemName + ");\n\n");
			b.append("// Add dummy item!\n");
			b.append("final var " + dummyItemName + " = new DataTreeNode(\"\", null);\n");
			b.append(dummyItemName + ".setUserObject(new " + treeItem.getItemDTO().getName() + "(");
			b.append(treeItem.getItemDTO().getPKAttribute().getDomainAttribute().getEmptyItemDefaultValue());
			b.append("));\n\n");
			b.append(FIELD_NAME_TREE + ".addNodeToParent(" + dummyItemName + ", " + subParentItemName + ");\n");
		});

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
			final var dtoName = assocName + "TreeHelperDTO";
			final BoundaryMethod m = item.getDataFetchMethod();
			var methodSignature = "void add" + assocName + "(DataTreeNode parentItem, " + dtoName + " dto)";

			// Add the method to fetch the sub-items
			var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Add " + item.getItemDTO().getDomainObject().getLabel() + " items to parent\n");
			b.append(" * @param parentItem\n");
			b.append(" * @param dto\n");
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
			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));\n");
			b.append("statusPanel.setBusy(true);\n\n");
			b.append(FIELD_NAME_TREE + ".removeChildNodes(parentItem, " + item.getItemDTO().getName() + ".class);\n\n");
			b.append("final java.util.List<" + item.getItemDTO().getName() + "> items = ");

			new ServiceInvocationGenerator(m, b).addInvocation("dto.getParentId()");

			final var compName = assocName + "Comparator";

			b.append("Collections.sort(items, new " + compName + "());\n");
			b.append("final var itemText = new StringBuilder();\n\n");
			b.append("for(final " + item.getItemDTO().getName() + " i : items)\n");
			b.append("{\n");
			b.append(addTreeItems("parentItem", item));
			b.append("}\n\n");
			b.append("// We have to fire the expand event once again!\n");
			b.append(FIELD_NAME_TREE + ".fireTreeExpanded(new TreePath(parentItem.getPath()));\n");

			importPackage("javax.swing.tree");

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Error while fetching data!", "e");

			b.append("\n");
			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
			b.append("statusPanel.setBusy(false);\n");
			b.append("JOptionPane.showMessageDialog(this, ");
			b.append(i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
			b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_data_fetch", "Fetch data"));
			b.append(", JOptionPane.ERROR_MESSAGE);\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");

			declarationGenerator.addCloseStatement();

			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
			b.append("statusPanel.setBusy(false);\n");
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
					importPackage("net.codecadenza.runtime.richclient.swing.dialog");

					methodSignature = "boolean " + methodName + "(" + dtoName + " dto)";

					b = new StringBuilder();
					b.append("/**\n");
					b.append(" * Add " + item.getItemDTO().getDomainObject().getLabel() + " to ");
					b.append(parentItem.getItemDTO().getDomainObject().getLabel() + "\n");
					b.append(" * @param dto\n");
					b.append(" * @return true if operation was finished successfully!\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private " + methodSignature + "\n");
					b.append("{\n");
					b.append("final var dlg = new " + addForm.getName() + "(" + tree.getName() + ".this, dto.getParentId());\n");
					b.append("dlg.setVisible(true);\n\n");
					b.append("return dlg.getReturnCode() == JTitleAreaDialog.RETURN_CODE_OK;\n");
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
		b.append("menu" + itemDomainObject.getName() + " = new JPopupMenu();\n\n");

		// Check if a form of type 'ADD' exists
		Form addForm = null;
		String fetchMethodName = null;

		if (treeItem.isRootItem()) {
			fetchMethodName = "add" + treeItem.getItemDTO().getDomainObject().getNamePlural() + "OfParent"
					+ treeItem.getItemDTO().getDomainObject().getName();

			if (tree.getRecursiveMethod() != null)
				addForm = getAddForm(treeItem);
		}

		if (addForm != null) {
			final var s = new StringBuilder();
			s.append("// Add menu item to add a new item\n");
			s.append("final var itemAdd = new JMenuItem(" + i18n.getI18NMessage("action_name_add", "Add item"));
			s.append(", ImageLoader.getImage(ImageLoader.NEW_DATA));\n\n");
			s.append("itemAdd.addActionListener(actionEvent ->\n");
			s.append("{\n");
			s.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
			s.append("if(selItem == null || selItem.getUserObject() == null)\n");
			s.append("return;\n\n");
			s.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getUserObject();\n\n");
			s.append("if(add" + itemDomainObject.getName() + "(dto))\n");
			s.append("{\n");
			s.append("final var parent = (" + itemDomainObject.getName() + "TreeHelperDTO) selItem.getHelperObject();\n\n");
			s.append("if(!parent.isDataLoaded())\n");
			s.append("return;\n\n");
			s.append(fetchMethodName + "(selItem, parent);\n");
			s.append("parent.setDataLoaded(true);\n");
			s.append(FIELD_NAME_TREE + ".reloadNode(selItem);\n");
			s.append("}\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".add(itemAdd);\n");

			b.append(securityHelper.wrapSecurityCode(addForm.getRoles(), s.toString()));
			b.append("\n");
		}

		// Check if a form of type 'UPDATE' exists
		final Form editForm = getEditForm(treeItem);

		if (editForm != null) {
			final var s = new StringBuilder();
			s.append("// Add menu item to edit item\n");
			s.append("final var itemEdit = new JMenuItem(" + i18n.getI18NMessage("action_name_edit", "Edit"));
			s.append(", ImageLoader.getImage(ImageLoader.EDIT_DATA));\n\n");
			s.append("itemEdit.addActionListener(actionEvent ->\n");
			s.append("{\n");
			s.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
			s.append("if(selItem == null || selItem.getUserObject() == null)\n");
			s.append("return;\n\n");
			s.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getUserObject();\n");
			s.append("edit" + itemDomainObject.getName() + "(dto);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".add(itemEdit);\n");

			b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), s.toString()));
			b.append("\n");
		}

		// Check if a form of type 'READONLY' exists
		final Form viewForm = getReadOnlyForm(treeItem);

		if (viewForm != null) {
			final var s = new StringBuilder();
			s.append("// Add menu item to view item\n");
			s.append("final var itemView = new JMenuItem(" + i18n.getI18NMessage("action_name_view", "View"));
			s.append(", ImageLoader.getImage(ImageLoader.VIEW_DATA));\n\n");
			s.append("itemView.addActionListener(actionEvent ->\n");
			s.append("{\n");
			s.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
			s.append("if(selItem == null || selItem.getUserObject() == null)\n");
			s.append("return;\n\n");
			s.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getUserObject();\n");
			s.append("view" + itemDomainObject.getName() + "(dto);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".add(itemView);\n");

			b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), s.toString()));
			b.append("\n");
		}

		// Add context menu items for file download operations
		for (final DomainAttribute attr : itemDomainObject.getAllLobAttributes()) {
			final BoundaryMethod downloadMethod = getDownloadMethod(itemDomainObject, attr);
			var methodName = "";

			if (downloadMethod == null)
				continue;

			if (attr.getDomainObject().equals(itemDomainObject))
				methodName = "download" + attr.getUpperCaseName();
			else
				methodName = "download" + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName();

			final var menuItemName = methodName + "Item";

			final var fb = new StringBuilder();
			fb.append("// Add menu item to download " + attr.getLabel() + "\n");
			fb.append("final var " + menuItemName + " = new JMenuItem(" + i18n.getI18NMessage("action_name_download", "Download"));
			fb.append(", ImageLoader.getImage(ImageLoader.DOWNLOAD));\n\n");
			fb.append(menuItemName + ".addActionListener(actionEvent ->\n");
			fb.append("{\n");
			fb.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
			fb.append("if(selItem == null || selItem.getUserObject() == null)\n");
			fb.append("return;\n\n");
			fb.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getUserObject();\n");
			fb.append(methodName + "(dto);\n");
			fb.append("});\n\n");
			fb.append("menu" + itemDomainObject.getName() + ".add(" + menuItemName + ");\n");

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
			final var s = new StringBuilder();
			s.append("// Add menu item to remove item\n");
			s.append("final var itemRemove = new JMenuItem(" + i18n.getI18NMessage("action_name_delete", "Delete"));
			s.append(", ImageLoader.getImage(ImageLoader.DELETE));\n\n");
			s.append("itemRemove.addActionListener(actionEvent ->\n");
			s.append("{\n");
			s.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
			s.append("if(selItem == null || selItem.getUserObject() == null)\n");
			s.append("return;\n\n");
			s.append("final var dto = (" + treeItem.getItemDTO().getName() + ") selItem.getUserObject();\n");
			s.append("final boolean success = delete" + itemDomainObject.getName() + "(dto);\n\n");
			s.append("if(success)\n");
			s.append(FIELD_NAME_TREE + ".removeNode(selItem);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".add(itemRemove);\n");

			if (addSecurity && deleteMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(deleteMethod.getRoles(), s.toString()));
			else
				b.append(s.toString());

			b.append("\n");
		}

		if (removeMethod != null) {
			final var parentDTOName = treeItem.getAssociation().getUpperCaseName() + "TreeHelperDTO";
			final String dtoName = treeItem.getItemDTO().getName();

			final var s = new StringBuilder();
			s.append("// Add menu item to remove item from collection\n");
			s.append("final var itemRemove = new JMenuItem(" + i18n.getI18NMessage("action_name_remove_item", "Remove item"));
			s.append(", ImageLoader.getImage(ImageLoader.DELETE));\n\n");
			s.append("itemRemove.addActionListener(actionEvent ->\n");
			s.append("{\n");
			s.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
			s.append("if(selItem == null || selItem.getUserObject() == null)\n");
			s.append("return;\n\n");
			s.append("final var item = (" + dtoName + ") selItem.getUserObject();\n");
			s.append("final var parentNode = (DataTreeNode) selItem.getParent();\n");
			s.append("final var parent = (" + parentDTOName + ") parentNode.getHelperObject();\n");
			s.append("final boolean success = " + removeMethod.getName() + "(parent, item);\n\n");
			s.append("if(success)\n");
			s.append(FIELD_NAME_TREE + ".removeNode(selItem);\n");
			s.append("});\n\n");
			s.append("menu" + itemDomainObject.getName() + ".add(itemRemove);\n");

			if (addSecurity && removeMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(removeMethod.getRoles(), s.toString()));
			else
				b.append(s.toString());

			b.append("\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add the method to build the tree
	 * @param formName
	 */
	private void addTreeBuildMethod(String formName) {
		final var b = new StringBuilder();
		final BoundaryBean boundaryBean = tree.getBoundaryMethod().getBoundaryBean();
		final var methodSignature = "void buildTree()";
		final var declarationGenerator = new ServiceDeclarationGenerator(this, boundaryBean, b);

		b.append("/**\n");
		b.append(" * Method to build and refresh tree view content\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("// Disable possibility to start thread twice!\n");
		b.append("if(dataFetchThread != null && !dataFetchThread.isDone())\n");
		b.append("return;\n\n");

		declarationGenerator.addLocalVariable();

		b.append("actionStopSearch.setEnabled(true);\n");
		b.append("actionRefresh.setEnabled(false);\n");

		if (addAdvSearch)
			b.append("actionAdvSearch.setEnabled(false);\n");

		b.append("counter = 0;\n");
		b.append("statusPanel.setBusy(true);\n");

		// Disable all quick-search fields
		tree.getQuickSearchItems().forEach(item -> {
			final var txtName = "txt" + item.getDTOAttribute().getUpperCaseName();

			b.append(txtName + ".setEnabled(false);\n");
		});

		final var msgKey = tree.getName() + "_root_item";
		String rootLabel = rootItem.getItemDTO().getDomainObject().getLabel();
		rootLabel = rootLabel.substring(0, 1).toUpperCase() + rootLabel.substring(1);

		b.append("\nrootNode = new DataTreeNode(");
		b.append(i18n.getI18NMessage(msgKey, rootLabel) + ", ImageLoader.getImage(ImageLoader.FOLDER));\n\n");
		b.append("dataFetchThread = new SwingWorker<>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.SwingWorker#doInBackground()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public Void doInBackground() throws Exception\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + rootItem.getItemDTO().getDomainObject().getLabel() + " items");

		b.append("\n");
		b.append("final java.util.List<" + rootItem.getItemDTO().getName() + "> items = ");

		if (tree.needsSearchObject())
			new ServiceInvocationGenerator(tree.getBoundaryMethod(), b).addInvocation("searchObj");
		else
			new ServiceInvocationGenerator(tree.getBoundaryMethod(), b).addInvocation();

		final var compName = rootItem.getItemDTO().getDomainObject().getName() + "Comparator";

		b.append("Collections.sort(items, new " + compName + "());\n\n");
		b.append("counter = items.size();\n");
		b.append("final var itemText = new StringBuilder();\n\n");
		b.append("for(final " + rootItem.getItemDTO().getName() + " i : items)\n");
		b.append("{\n");
		b.append(addTreeItems(FIELD_NAME_TREE, rootItem));
		b.append("}\n\n");
		b.append("return null;\n");
		b.append("}\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.SwingWorker#done()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void done()\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("// We have to call method get() in order to see any exception!\n");
		b.append("get();\n\n");
		b.append("statusPanel.setMessage(" + i18n.getI18NMessage("msg_tree_data_fetch", "{0} item(s) fetched", "counter") + ");\n");
		b.append("}\n");
		b.append("catch (final CancellationException ex)\n");
		b.append("{\n");
		b.append("statusPanel.setMessage(" + i18n.getI18NMessage("status_operation_cancel", "Operation canceled") + ");\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");
		b.append("if(ex instanceof InterruptedException)\n");
		b.append("Thread.currentThread().interrupt();\n\n");

		addErrorLog(b, "Error while fetching data!", "ex");

		b.append("\n");
		b.append("JOptionPane.showMessageDialog(" + formName + ".this, ");
		b.append(i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
		b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_data_fetch", "Fetch data"));
		b.append(", JOptionPane.ERROR_MESSAGE);\n");
		b.append("}\n\n");

		declarationGenerator.addCloseStatement();

		b.append("statusPanel.setBusy(false);\n");
		b.append("actionStopSearch.setEnabled(false);\n");
		b.append("actionRefresh.setEnabled(true);\n");

		if (addAdvSearch)
			b.append("actionAdvSearch.setEnabled(true);\n");

		// Enable all quick-search fields
		tree.getQuickSearchItems().forEach(item -> {
			final var txtName = "txt" + item.getDTOAttribute().getUpperCaseName();

			b.append(txtName + ".setEnabled(true);\n");
		});

		b.append(FIELD_NAME_TREE + ".refreshTreeModel(rootNode);\n");
		b.append("}\n");
		b.append("};\n\n");
		b.append("dataFetchThread.execute();\n");
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
		final var methodSignature = "void " + fetchMethodName + "(DataTreeNode parentItem, " + paramDTOName + " dto)";
		final var declarationGenerator = new ServiceDeclarationGenerator(this, tree.getRecursiveMethod(), b);

		b.append("/**\n");
		b.append(" * @param parentItem\n");
		b.append(" * @param dto\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Perform data fetch operation for " + rootItem.getItemDTO().getDomainObject().getLabel() + " sub-items");

		b.append("\n");
		b.append("setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));\n");
		b.append("statusPanel.setBusy(true);\n\n");

		declarationGenerator.addLocalVariable();

		b.append("\n");
		b.append(FIELD_NAME_TREE + ".removeChildNodes(parentItem, " + dtoName + ".class);\n\n");
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
		b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
		b.append("statusPanel.setBusy(false);\n");
		b.append("JOptionPane.showMessageDialog(this, ");
		b.append(i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
		b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_data_fetch", "Fetch data"));
		b.append(", JOptionPane.ERROR_MESSAGE);\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");

		declarationGenerator.addCloseStatement();

		b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
		b.append("statusPanel.setBusy(false);\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Add methods to listen to status change events
	 */
	private void addStatusImpMethods() {
		StringBuilder b;
		var methodSignature = "void setBusy(boolean busy)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.StatusReceivable#setBusy(boolean)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("statusPanel.setBusy(busy);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void setStatusErrorMessage(String msg)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.");
		b.append("StatusReceivable#setStatusErrorMessage(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("statusPanel.setMessage(msg);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void setStatusIcon(ImageIcon icon)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.");
		b.append("StatusReceivable#setStatusIcon(javax.swing.ImageIcon)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void setStatusInfoMessage(String msg)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.");
		b.append("StatusReceivable#setStatusInfoMessage(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("statusPanel.setMessage(msg);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Method to create actions and action initializer
	 */
	private void addActions() {
		final DomainObject rootDomainObject = rootItem.getItemDTO().getDomainObject();
		var b = new StringBuilder();
		var className = "RefreshAction";

		b.append("/**\n");
		b.append(" * Create refresh action\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private class " + className + " extends AbstractAction\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public " + className + "()\n");
		b.append("{\n");
		b.append("super(" + i18n.getI18NMessage("action_name_refresh", "Refresh"));
		b.append(", ImageLoader.getImage(ImageLoader.REFRESH));\n");
		b.append("}\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void actionPerformed(ActionEvent event)\n");
		b.append("{\n");
		b.append("buildTree();\n");
		b.append("}\n");
		b.append("}\n\n");

		addSubClass(className, b.toString());

		if (addAdvSearch) {
			className = "AdvancedSearchAction";

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Action to perform advanced search\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private class " + className + " extends AbstractAction\n");
			b.append("{\n");
			b.append("private static final long serialVersionUID = 1L;\n\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append("public " + className + "()\n");
			b.append("{\n");
			b.append("super(" + i18n.getI18NMessage("action_name_adv_search", "Advanced search"));
			b.append(", ImageLoader.getImage(ImageLoader.SEARCH));\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void actionPerformed(ActionEvent event)\n");
			b.append("{\n");
			b.append("SearchDTO searchObjAdv;\n");
			b.append("final String viewID = " + tree.getName() + ".class.getName();\n\n");
			b.append("if(SearchManager.getLastSearch(viewID) == null)\n");
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
			b.append("searchObjAdv = SearchManager.getLastSearch(viewID);\n\n");
			b.append("final var dlg = new SearchInputDialog(searchObjAdv, " + tree.getName() + ".this);\n");
			b.append("dlg.setModal(true);\n");
			b.append("dlg.setVisible(true);\n\n");
			b.append("if(dlg.getReturnCode() == JTitleAreaDialog.RETURN_CODE_OK)\n");
			b.append("{\n");
			b.append("searchObjAdv = dlg.getSearchInput();\n");
			b.append("searchObj = searchObjAdv;\n\n");
			b.append("SearchManager.saveLastSearch(viewID, searchObjAdv);\n");
			b.append("buildTree();\n");
			b.append("}\n");
			b.append("}\n");
			b.append("}\n\n");

			addSubClass(className, b.toString());
		}

		// Add the action for creating new objects
		final Form createForm = getCreateNewForm(rootItem);

		if (createForm != null) {
			className = "Create" + rootDomainObject.getName() + "Action";

			importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);
			importPackage("net.codecadenza.runtime.richclient.swing.dialog");

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Action to create new " + rootDomainObject.getLabel() + "\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("private class " + className + " extends AbstractAction\n");
			b.append("{\n");
			b.append("private static final long serialVersionUID = 1L;\n\n");
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" */\n");
			b.append("public " + className + "()\n");
			b.append("{\n");
			b.append("super(" + i18n.getI18NMessage("action_name_create", "Create"));
			b.append(", ImageLoader.getImage(ImageLoader.NEW_DATA));\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void actionPerformed(ActionEvent event)\n");
			b.append("{\n");
			b.append("final var dlg = new " + createForm.getName() + "(" + tree.getName() + ".this);\n");
			b.append("dlg.setVisible(true);\n\n");
			b.append("if(dlg.getReturnCode() == JTitleAreaDialog.RETURN_CODE_OK)\n");
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
		b.append("private class " + className + " extends AbstractAction\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append("public " + className + "()\n");
		b.append("{\n");
		b.append("super(" + i18n.getI18NMessage("action_name_stop_search", "Stop search"));
		b.append(", ImageLoader.getImage(ImageLoader.STOP));\n\n");
		b.append("setEnabled(false);\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void actionPerformed(ActionEvent event)\n");
		b.append("{\n");
		b.append("if(dataFetchThread != null && !dataFetchThread.isDone())\n");
		b.append("dataFetchThread.cancel(true);\n");
		b.append("}\n");
		b.append("}\n\n");

		addSubClass(className, b.toString());

		final var methodSignature = "void createActions()";

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
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Create the drop listener
	 */
	private void createDropListener() {
		final var b = new StringBuilder();
		final var methodSignature = "void initDrop()";

		if (dropItems.isEmpty())
			return;

		b.append("/**\n");
		b.append(" * Initialize drop capability of tree\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var drop = new DropTarget(tree, new DropTargetListener()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.dnd.DropTargetListener#dragEnter(java.awt.dnd.DropTargetDragEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void dragEnter(DropTargetDragEvent dropEvent)\n");
		b.append("{\n");
		b.append("boolean flavorFound = false;\n");
		b.append("final DataFlavor[] fl = dropEvent.getTransferable().getTransferDataFlavors();\n\n");
		b.append("for(final DataFlavor f : fl)\n");
		b.append("if(f.equals(DataTreeNode.NODE_FLAVOR))\n");
		b.append("{\n");
		b.append("flavorFound = true;\n");
		b.append("break;\n");
		b.append("}\n\n");
		b.append("if(!flavorFound)\n");
		b.append("dropEvent.rejectDrag();\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.dnd.DropTargetListener#dragExit(java.awt.dnd.DropTargetEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void dragExit(DropTargetEvent dropEvent)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.dnd.DropTargetListener#dragOver(java.awt.dnd.DropTargetDragEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void dragOver(DropTargetDragEvent dropEvent)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.dnd.DropTargetListener#drop(java.awt.dnd.DropTargetDropEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void drop(DropTargetDropEvent dropEvent)\n");
		b.append("{\n");
		b.append("final Point dropPoint = dropEvent.getLocation();\n");
		b.append("final TreePath dropPath = " + FIELD_NAME_TREE + ".getClosestPathForLocation(dropPoint.x, dropPoint.y);\n\n");
		b.append("if(dropPath == null)\n");
		b.append("return;\n\n");
		b.append("final var itemDrop = (DataTreeNode) dropPath.getLastPathComponent();\n");
		b.append("final DataTreeNode itemDrag = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");

		boolean isFirstItem = true;

		for (final TreeViewItem item : dropItems) {
			if (isFirstItem) {
				b.append("if");
				isFirstItem = false;
			}
			else
				b.append("else if");

			final String dtoName = item.getItemDTO().getName();
			var helperDTOName = "";

			b.append("(itemDrag.getUserObject() instanceof final " + dtoName + " dragDTO && ");

			if (item.isRootItem())
				b.append("itemDrop.getUserObject() instanceof final " + dtoName + " dropDTO)\n");
			else {
				final String assocName = item.getAssociation().getUpperCaseName();
				helperDTOName = assocName + "TreeHelperDTO";

				b.append("itemDrop.getHelperObject() instanceof final " + helperDTOName + " dropDTO)\n");
			}

			b.append("{\n");

			final var fb = new StringBuilder();
			final String pkAttrGetter = item.getItemDTO().getPKAttribute().getGetterName();
			String pkParentAttrGetter = pkAttrGetter;

			if (!item.isRootItem())
				pkParentAttrGetter = "getParentId()";

			final DTOBeanAttribute pkAttr = item.getItemDTO().getPKAttribute();
			final JavaType pkType = pkAttr.getDomainAttribute().getJavaType();
			final BoundaryMethod dropMethod = item.getDropMethod();
			final var declarationGenerator = new ServiceDeclarationGenerator(this, dropMethod, fb);

			declarationGenerator.addLocalVariable();

			fb.append("\n");
			fb.append("try\n");
			fb.append("{\n");
			fb.append("dropEvent.dropComplete(true);\n\n");

			if (item.isRootItem()) {
				fb.append("// Avoid cyclic reference!\n");

				if (pkType.isPrimitive())
					fb.append("if(dragDTO." + pkAttrGetter + " == dropDTO." + pkParentAttrGetter + ")\n");
				else
					fb.append("if(dragDTO." + pkAttrGetter + ".equals(dropDTO." + pkParentAttrGetter + "))\n");

				fb.append("return;\n\n");

				new ServiceInvocationGenerator(dropMethod, fb).addInvocation("dragDTO." + pkAttrGetter, "dropDTO." + pkParentAttrGetter);

				fb.append("\n");
				fb.append("final var itemDragParent = (DataTreeNode) itemDrag.getParent();\n\n");
				fb.append("itemDragParent.remove(itemDrag);\n\n");
				fb.append("itemDrop.add(itemDrag);\n");
				fb.append(FIELD_NAME_TREE + ".updateUI();\n");
				fb.append("}\n");
				fb.append("catch (final Exception e)\n");
				fb.append("{\n");
			}
			else if (item.getAssociation() instanceof OneToManyAssociation) {
				new ServiceInvocationGenerator(dropMethod, fb).addInvocation("dragDTO." + pkAttrGetter, "dropDTO." + pkParentAttrGetter);

				fb.append("\n");
				fb.append("final var itemDragParent = (DataTreeNode) itemDrag.getParent();\n");
				fb.append("itemDragParent.remove(itemDrag);\n\n");
				fb.append("itemDrop.add(itemDrag);\n");
				fb.append(FIELD_NAME_TREE + ".updateUI();\n");
				fb.append("}\n");
				fb.append("catch (final Exception e)\n");
				fb.append("{\n");
			}
			else {
				new ServiceInvocationGenerator(dropMethod, fb).addInvocation("dropDTO." + pkParentAttrGetter, "dragDTO." + pkAttrGetter);

				fb.append("\n");
				fb.append("// Refresh parent node\n");
				fb.append("add" + item.getAssociation().getUpperCaseName() + "(itemDrop, dropDTO);\n");
				fb.append("}\n");
				fb.append("catch (final DuplicateCollectionEntryException e)\n");
				fb.append("{\n");
				fb.append("JOptionPane.showMessageDialog(" + tree.getName() + ".this, ");
				fb.append(i18n.getI18NMessage("msg_err_duplicate_entry", "Duplicate entry in list is disallowed!"));
				fb.append(", " + i18n.getI18NMessage("msg_title_drop_op", "Drop operation") + ", JOptionPane.WARNING_MESSAGE);\n");
				fb.append("}\n");
				fb.append("catch (final Exception e)\n");
				fb.append("{\n");

				addErrorLog(fb, "Drop operation failed!", "e");

				fb.append("\n");
			}

			fb.append("JOptionPane.showMessageDialog(" + tree.getName() + ".this, ");
			fb.append(i18n.getI18NMessage("msg_err_drop_op", "Drop operation failed! Message: "));
			fb.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_drop_op", "Drop operation"));
			fb.append(", JOptionPane.ERROR_MESSAGE);\n");
			fb.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();

			if (addSecurity && dropMethod.getPermissionMode() != PermissionModeEnumeration.PERMIT_ALL)
				b.append(securityHelper.wrapSecurityCode(dropMethod.getRoles(), fb.toString()));
			else
				b.append(fb);

			b.append("}\n");
		}

		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.dnd.DropTargetListener#dropActionChanged(java.awt.dnd.DropTargetDragEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void dropActionChanged(DropTargetDragEvent dropEvent)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n");
		b.append("});\n\n");
		b.append("drop.setActive(true);\n");
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

		b.append("/**\n");
		b.append(" * Add key listener to tree view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append(FIELD_NAME_TREE + ".addKeyListener(new KeyAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.KeyAdapter#keyReleased(java.awt.event.KeyEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void keyReleased(KeyEvent e)\n");
		b.append("{\n");
		b.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
		b.append("if(selItem == null || selItem.getUserObject() == null)\n");
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
		b.append("if(e.getKeyCode() == KeyEvent.VK_ENTER)\n");
		b.append("{\n");

		boolean isFirstDTO = true;

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

				b.append("(selItem.getUserObject() instanceof final " + dto.getName() + " dto)\n");
				b.append("{\n");

				var methodName = "";

				if (editForm != null) {
					methodName = "edit" + dto.getDomainObject().getName();

					if (addSecurity && viewForm != null)
						b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\nreturn;\n"));
					else
						b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\n"));

					if (addSecurity && viewForm != null) {
						methodName = "view" + dto.getDomainObject().getName();

						b.append("\n");
						b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), methodName + "(dto);\n"));
					}
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

		b.append("if(e.getKeyCode() == KeyEvent.VK_DELETE)\n");
		b.append("{\n");

		// Add the delete method for the root tree item
		final BoundaryMethod deleteMethodOfRootItem = getDeleteMethod(rootItem);
		boolean firstIf = true;

		if (deleteMethodOfRootItem != null) {
			final var methodName = "delete" + rootItem.getItemDTO().getDomainObject().getName();
			firstIf = false;

			b.append("if(selItem.getUserObject() instanceof final " + rootItem.getItemDTO().getName() + " dto)\n");
			b.append("{\n");

			final var s = new StringBuilder();
			s.append("final boolean success = " + methodName + "(dto);\n\n");
			s.append("if(success)\n");
			s.append(FIELD_NAME_TREE + ".removeNode(selItem);\n");

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
				final var parentDTOName = entry.getKey().getAssociation().getUpperCaseName() + "TreeHelperDTO";

				if (firstIf)
					firstIf = false;
				else
					b.append("else ");

				b.append("if(selItem.getUserObject() instanceof final " + dto.getName() + " item");
				b.append(" && ((DataTreeNode)selItem.getParent()).getHelperObject() instanceof final " + parentDTOName + " parent)\n");
				b.append("{\n");

				final var s = new StringBuilder();
				s.append("final boolean success = " + removeMethod.getName() + "(parent, item);\n\n");
				s.append("if(success)\n");
				s.append(FIELD_NAME_TREE + ".removeNode(selItem);\n");

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

				b.append("if(selItem.getUserObject() instanceof final " + dto.getName() + " dto)\n");
				b.append("{\n");

				final var s = new StringBuilder();
				s.append("final boolean success = " + methodName + "(dto);\n\n");
				s.append("if(success)\n");
				s.append(FIELD_NAME_TREE + ".removeNode(selItem);\n");

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
	 * Add the tree listener method
	 * @param rootItem
	 */
	private void createTreeListener(TreeViewItem rootItem) {
		final var b = new StringBuilder();
		final var methodSignature = "void addTreeListener()";

		b.append("/**\n");
		b.append(" * Add tree listener to tree view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append(FIELD_NAME_TREE + ".addTreeExpansionListener(new TreeExpansionListener()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.event.TreeExpansionListener#treeCollapsed(javax.swing.event.TreeExpansionEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void treeCollapsed(TreeExpansionEvent e)\n");
		b.append("{\n");
		b.append("// No implementation required!\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.event.TreeExpansionListener#treeExpanded(javax.swing.event.TreeExpansionEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void treeExpanded(TreeExpansionEvent e)\n");
		b.append("{\n");

		boolean firstIf = true;

		for (final String assocName : helperDTOMap.keySet()) {
			final var dtoName = assocName + "TreeHelperDTO";

			if (firstIf) {
				firstIf = false;
				b.append("final var selItem = (DataTreeNode) e.getPath().getLastPathComponent();\n\n");
				b.append("if");
			}
			else
				b.append("else if");

			b.append("(selItem.getHelperObject() instanceof final " + dtoName + " dto && !dto.isDataLoaded())\n");
			b.append("{\n");
			b.append("dto.setDataLoaded(true);\n");

			final var fetchMethodName = "add" + assocName.substring(0, 1).toUpperCase() + assocName.substring(1);

			b.append(fetchMethodName + "(selItem, dto);\n");
			b.append("}\n");
		}

		if (tree.getRecursiveMethod() != null) {
			if (firstIf) {
				b.append("final var selItem = (DataTreeNode) e.getPath().getLastPathComponent();\n\n");
				b.append("if");
			}
			else
				b.append("else if");

			final String domainObjectName = rootItem.getItemDTO().getDomainObject().getName();
			final var dtoName = domainObjectName + "TreeHelperDTO";

			b.append("(selItem.getHelperObject() instanceof final " + dtoName + " dto && !dto.isDataLoaded())\n");
			b.append("{\n");
			b.append("dto.setDataLoaded(true);\n");

			final var fetchMethodName = "add" + rootItem.getItemDTO().getDomainObject().getNamePlural() + "OfParent"
					+ rootItem.getItemDTO().getDomainObject().getName();

			b.append(fetchMethodName + "(selItem, dto);\n\n");
			b.append("// We have to fire the expand event once again! Otherwise the node won't be expanded!\n");
			b.append(FIELD_NAME_TREE + ".fireTreeExpanded(new TreePath(selItem.getPath()));\n");

			importPackage("javax.swing.tree");

			b.append("}\n");
		}

		if (helperDTOMap.isEmpty() && tree.getRecursiveMethod() == null)
			b.append("// No implementation required!\n");

		b.append("}\n");
		b.append("});\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
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
		b.append(FIELD_NAME_TREE + ".addMouseListener(new MouseAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void mousePressed(MouseEvent e)\n");
		b.append("{\n");
		b.append("if(e.isPopupTrigger())\n");
		b.append("{\n");
		b.append("showMenu(e);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.MouseAdapter#mouseReleased(java.awt.event.MouseEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void mouseReleased(MouseEvent e)\n");
		b.append("{\n");
		b.append("if(e.isPopupTrigger())\n");
		b.append("{\n");
		b.append("showMenu(e);\n");
		b.append("}\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * Show menu\n");
		b.append(" * @param e\n");
		b.append(" */\n");
		b.append("public void showMenu(MouseEvent e)\n");
		b.append("{\n");
		b.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
		b.append("if(selItem == null)\n");
		b.append("return;\n\n");

		for (final DTOBean dto : distinctDTOSet) {
			if (isFirstDTO) {
				b.append("if");
				isFirstDTO = false;
			}
			else
				b.append("else if");

			final var menuName = "menu" + dto.getDomainObject().getName();

			b.append("(selItem.getUserObject() instanceof " + dto.getName() + ")\n");
			b.append(menuName + ".show(e.getComponent(), e.getX(), e.getY());\n");
		}

		helperDTOMap.keySet().forEach(assocName -> {
			final var dtoName = assocName + "TreeHelperDTO";
			final var menuName = "menu" + assocName;

			b.append("else if(selItem.getHelperObject() instanceof " + dtoName + ")\n");
			b.append(menuName + ".show(e.getComponent(), e.getX(), e.getY());\n");
		});

		b.append("}\n");

		if (hasUpdateOrReadonlyForm()) {
			b.append("\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void mouseClicked(MouseEvent e)\n");
			b.append("{\n");
			b.append("if(e.getClickCount() != 2)\n");
			b.append("return;\n\n");
			b.append("final DataTreeNode selItem = " + FIELD_NAME_TREE + ".getSelectedTreeNode();\n\n");
			b.append("if(selItem == null || selItem.getUserObject() == null)\n");
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

					b.append("(selItem.getUserObject() instanceof final " + dto.getName() + " dto)\n");
					b.append("{\n");

					var methodName = "";

					if (editForm != null) {
						methodName = "edit" + dto.getDomainObject().getName();

						if (addSecurity && viewForm != null)
							b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\nreturn;\n"));
						else
							b.append(securityHelper.wrapSecurityCode(editForm.getRoles(), methodName + "(dto);\n"));

						if (addSecurity && viewForm != null) {
							methodName = "view" + dto.getDomainObject().getName();

							b.append("\n");
							b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), methodName + "(dto);\n"));
						}
					}
					else {
						methodName = "view" + dto.getDomainObject().getName();

						b.append(securityHelper.wrapSecurityCode(viewForm.getRoles(), methodName + "(dto);\n"));
					}

					b.append("}\n");
				}
			}

			b.append("}\n");
		}

		b.append("});\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
