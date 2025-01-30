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

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_PLUGIN_ID;
import static net.codecadenza.eclipse.shared.Constants.NAV_VIEW_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_TREE;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_VIEW;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the application tree navigator of an Eclipse RCP/RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseNavigatorGenerator extends AbstractNavigatorGenerator {
	private static final String ROOT_NODE_NAME = "tree";

	private final RichClientI18NGenerator i18n;
	private int viewIndex = 1;

	/**
	 * Constructor
	 * @param project
	 */
	public EclipseNavigatorGenerator(Project project) {
		super(project, new EclipseSecurityHelper(project));

		this.i18n = new RichClientI18NGenerator(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getClientNamespace().toString() + PACK_CLIENT_VIEW;

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, NAV_VIEW_NAME, packageName);
		javaFile.setComment("Application tree view navigator");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importClass("jakarta.annotation.PostConstruct");
		importClass("jakarta.inject.Inject");
		importClass("org.eclipse.jface.dialogs.MessageDialog");
		importClass("org.eclipse.swt.SWT");
		importPackage("org.eclipse.swt.events");
		importClass("org.eclipse.swt.layout.GridData");
		importClass("org.eclipse.swt.layout.GridLayout");
		importPackage("org.eclipse.swt.widgets");
		importPackage("org.eclipse.e4.ui.workbench.modeling");
		importPackage("org.eclipse.e4.ui.model.application.ui");
		importPackage("org.eclipse.e4.ui.model.application.ui.basic");
		importPackage("net.codecadenza.runtime.richclient.search.event");
		importClass("net.codecadenza.runtime.search.dto.SearchListDTO");
		importClass("net.codecadenza.runtime.richclient.eclipse.image.ImageCache");
		importClass("net.codecadenza.runtime.richclient.eclipse.search.AbstractSearchResultView");
		importPackage("net.codecadenza.runtime.richclient.search.util");

		if (hasTree)
			importPackage(project.getClientNamespace() + PACK_CLIENT_TREE);

		addImports(securityHelper.getSecurityImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + NAV_VIEW_NAME + " implements SearchDTOChangeListener");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPublicConstant(JavaType.STRING, "ID",
				"\"" + project.getClientNamespace().toString() + PACK_CLIENT_VIEW + "." + NAV_VIEW_NAME + "\"").create();
		addPrivateConstant(JavaType.STRING, "IMG_FOLDER_CLOSED", "\"folder.png\"").create();
		addPrivateConstant(JavaType.STRING, "IMG_FOLDER_OPEN", "\"folder_open.png\"").create();
		addPrivateConstant(JavaType.STRING, "IMG_VIEW", "\"window_view.png\"").create();
		addPrivateConstant(JavaType.STRING, "CONTR_URI_PREFIX", "\"bundleclass://" + DEFAULT_PLUGIN_ID + "/\"").create();
		addPrivateField("EPartService", "partService").withFinalModifier().create();
		addPrivateField("Tree", "tree").create();
		addPrivateField("Menu", "mnuUserDef").create();
		addPrivateField("TreeItem", "itemUserDefFolder").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		var b = new StringBuilder();
		var signature = NAV_VIEW_NAME + "()";

		b.append("/**\n");
		b.append(" * Default constructor\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + signature + "\n");
		b.append("{\n");
		b.append("this.partService = null;\n");
		b.append("}\n\n");

		addConstructor(signature, b.toString());

		b = new StringBuilder();
		signature = NAV_VIEW_NAME + "(EPartService partService)";

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param partService\n");
		b.append(" */\n");
		b.append("@Inject\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + signature + "\n");
		b.append("{\n");
		b.append("this.partService = partService;\n");
		b.append("}\n\n");

		addConstructor(signature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		var methodSignature = "void onNewSavedQuery(SearchListDTO dto)";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.search.event.SearchDTOChangeListener#");
		b.append("onNewSavedQuery(net.codecadenza.runtime.search.dto.SearchListDTO)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var itemUserDef = new TreeItem(itemUserDefFolder, SWT.NONE);\n");
		b.append("itemUserDef.setText(dto.getName());\n");
		b.append("itemUserDef.setData(dto);\n");
		b.append("itemUserDef.setImage(ImageCache.getImage(\"window_saved_search.png\"));\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "void createPartControl(Composite parent, Shell shell)";

		b.append("/**\n");
		b.append(" * Create contents of the view part\n");
		b.append(" * @param parent\n");
		b.append(" * @param shell\n");
		b.append(" */\n");
		b.append("@PostConstruct\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var container = new Composite(parent, SWT.NONE);\n");
		b.append("container.setLayout(new GridLayout());\n\n");
		b.append("// Register listener\n");
		b.append("SearchDTOChangeController.addSearchDTOChangeListener(this);\n\n");
		b.append("tree = new Tree(container, SWT.BORDER);\n\n");
		b.append("tree.addTreeListener(new TreeAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.TreeAdapter#treeCollapsed(org.eclipse.swt.events.TreeEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void treeCollapsed(TreeEvent e)\n");
		b.append("{\n");
		b.append("final var item = (TreeItem) e.item;\n");
		b.append("item.setImage(ImageCache.getImage(IMG_FOLDER_CLOSED));\n");
		b.append("}\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.TreeAdapter#treeExpanded(org.eclipse.swt.events.TreeEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void treeExpanded(TreeEvent e)\n");
		b.append("{\n");
		b.append("final var item = (TreeItem) e.item;\n");
		b.append("item.setImage(ImageCache.getImage(IMG_FOLDER_OPEN));\n");
		b.append("}\n");
		b.append("});\n\n");
		b.append("tree.addMenuDetectListener(event ->\n");
		b.append("{\n");
		b.append("tree.setMenu(null);\n\n");
		b.append("TreeItem selItem = null;\n\n");
		b.append("final TreeItem[] selItems = tree.getSelection();\n\n");
		b.append("for(final TreeItem item : selItems)\n");
		b.append("selItem = item;\n\n");
		b.append("if(selItem == null || selItem.getData() == null)\n");
		b.append("return;\n\n");
		b.append("if(selItem.getData() instanceof SearchListDTO)\n");
		b.append("tree.setMenu(mnuUserDef);\n");
		b.append("});\n\n");
		b.append("tree.addMouseListener(new MouseAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void mouseDoubleClick(MouseEvent e)\n");
		b.append("{\n");
		b.append("TreeItem selItem = null;\n\n");
		b.append("if(e.button != 1)\n");
		b.append("return;\n\n");
		b.append("final TreeItem[] selItems = tree.getSelection();\n\n");
		b.append("for(final TreeItem item : selItems)\n");
		b.append("selItem = item;\n\n");
		b.append("if(selItem == null || selItem.getData() == null)\n");
		b.append("return;\n\n");
		b.append("if(selItem.getData() instanceof final String viewId)\n");
		b.append("{\n");
		b.append("tree.setMenu(null);\n\n");
		b.append("for(final MPart part : partService.getParts())\n");
		b.append("if(part.getElementId() != null && part.getElementId().equals(viewId))\n");
		b.append("{\n");
		b.append("part.setVisible(true);\n\n");
		b.append("partService.activate(part);\n");
		b.append("break;\n");
		b.append("}\n");
		b.append("}\n");
		b.append("else\n");
		b.append("{\n");
		b.append("tree.setMenu(mnuUserDef);\n\n");
		b.append("final var dto = (SearchListDTO) selItem.getData();\n");
		b.append("final String viewId = dto.getViewName();\n\n");
		b.append("for(final MPart part : partService.getParts())\n");
		b.append("if(part.getElementId() != null && part.getElementId().equals(viewId))\n");
		b.append("{\n");
		b.append("final MElementContainer<MUIElement> container = part.getParent();\n\n");
		b.append("// Create new part to show saved query in a separate view\n");
		b.append("final MPart newPart = MBasicFactory.INSTANCE.createPart();\n");
		b.append("newPart.setContributionURI(CONTR_URI_PREFIX + viewId);\n");
		b.append("newPart.setCloseable(true);\n");
		b.append("newPart.getTransientData().put(AbstractSearchResultView.SAVED_QUERY_ID_KEY, dto.getId());\n");
		b.append("newPart.setVisible(true);\n");
		b.append("newPart.setLabel(dto.getName());\n\n");
		b.append("container.getChildren().add(newPart);\n\n");
		b.append("partService.activate(newPart);\n");
		b.append("break;\n");
		b.append("}\n");
		b.append("}\n");
		b.append("}\n");
		b.append("});\n\n");
		b.append("tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n");
		b.append(createNavigatorTreeStructure());
		b.append("\nitemUserDefFolder = new TreeItem(tree, SWT.NONE);\n");
		b.append("itemUserDefFolder.setText(" + i18n.getI18NMessage("nav_item_user_def", "User defined queries") + ");\n");
		b.append("itemUserDefFolder.setData(null);\n");
		b.append("itemUserDefFolder.setImage(ImageCache.getImage(IMG_FOLDER_CLOSED));\n\n");
		b.append("// Add user defined queries\n");
		b.append("for(final SearchListDTO dto : SearchManager.getAllSavedSearchObjects())\n");
		b.append("{\n");
		b.append("final var itemUserDef = new TreeItem(itemUserDefFolder, SWT.NONE);\n");
		b.append("itemUserDef.setText(dto.getName());\n");
		b.append("itemUserDef.setData(dto);\n");
		b.append("itemUserDef.setImage(ImageCache.getImage(\"window_saved_search.png\"));\n");
		b.append("}\n\n");
		b.append("mnuUserDef = new Menu(tree);\n\n");
		b.append("final var mniDelete = new MenuItem(mnuUserDef, SWT.NONE);\n");
		b.append("mniDelete.setImage(ImageCache.getImage(ImageCache.IMG_DELETE));\n\n");
		b.append("mniDelete.addSelectionListener(new SelectionAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void widgetSelected(final SelectionEvent e)\n");
		b.append("{\n");
		b.append("TreeItem selItem = null;\n\n");
		b.append("// Get selected item\n");
		b.append("final TreeItem[] selItems = tree.getSelection();\n\n");
		b.append("for(final TreeItem item : selItems)\n");
		b.append("selItem = item;\n\n");
		b.append("if(selItem == null)\n");
		b.append("return;\n\n");
		b.append("final boolean doIt = MessageDialog.openQuestion(shell, ");
		b.append(i18n.getI18NMessage("msg_title_delete_query", "Delete saved query"));
		b.append(", " + i18n.getI18NMessage("msg_conf_delete_query", "Do you really want to delete selected query?") + ");\n\n");
		b.append("if(!doIt)\n");
		b.append("return;\n\n");
		b.append("final var listDTO = (SearchListDTO) selItem.getData();\n");
		b.append("SearchManager.deleteSavedSearchObject(listDTO.getId());\n\n");
		b.append("selItem.dispose();\n");
		b.append("}\n");
		b.append("});\n\n");
		b.append("mniDelete.setText(" + i18n.getI18NMessage("mni_delete", "Delete") + ");\n");
		b.append("}\n");

		addMethod(methodSignature, b.toString());

		i18n.save();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator#getRootNodeName()
	 */
	@Override
	protected String getRootNodeName() {
		return ROOT_NODE_NAME;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator#addFormGroup(java.lang.String,
	 * java.lang.String, net.codecadenza.eclipse.model.client.FormGroup)
	 */
	@Override
	protected String addFormGroup(String parentNodeName, String nodeName, FormGroup group) {
		final var formGroupNode = new StringBuilder();
		formGroupNode.append("final var " + nodeName + " = new TreeItem(" + parentNodeName + ", SWT.NONE);\n");
		formGroupNode.append(nodeName + ".setText(" + i18n.getI18N(group) + ");\n");
		formGroupNode.append(nodeName + ".setData(null);\n");
		formGroupNode.append(nodeName + ".setImage(ImageCache.getImage(IMG_FOLDER_CLOSED));\n");
		formGroupNode.append(addGroupsToParent(nodeName, group));

		return formGroupNode.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.navigator.AbstractNavigatorGenerator#addView(java.lang.String,
	 * net.codecadenza.eclipse.model.client.Form)
	 */
	@Override
	protected String addView(String parentNodeName, Form form) {
		final var nodeName = "itemView" + indexFormat.format(viewIndex++);

		final var viewNode = new StringBuilder();
		viewNode.append("final var " + nodeName + " = new TreeItem(" + parentNodeName + ", SWT.NONE);\n");
		viewNode.append(nodeName + ".setText(" + i18n.getI18N(form) + ");\n");
		viewNode.append(nodeName + ".setData(" + form.getName() + ".ID);\n");
		viewNode.append(nodeName + ".setImage(ImageCache.getImage(IMG_VIEW));\n");

		return viewNode.toString();
	}

}
