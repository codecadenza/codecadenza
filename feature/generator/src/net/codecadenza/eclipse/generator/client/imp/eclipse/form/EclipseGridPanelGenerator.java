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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form;

import static net.codecadenza.eclipse.shared.Constants.ACTION_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.BUTTON_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_DLG;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import java.util.regex.Pattern;
import net.codecadenza.eclipse.generator.client.common.action.ActionHelper;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.file.EclipseFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for grid panels of an Eclipse RCP/RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseGridPanelGenerator extends AbstractJavaSourceGenerator {
	private static final Pattern ACTION_PREFIX_PATTERN = Pattern.compile(ACTION_PREFIX);

	private final FormPanel panel;
	private final Project project;
	private final RichClientI18NGenerator i18n;
	private final EclipseSecurityHelper securityHelper;
	private final String pkTypeName;

	/**
	 * Constructor
	 * @param panel
	 */
	public EclipseGridPanelGenerator(FormPanel panel) {
		super(panel.getSourceFile());

		this.panel = panel;
		this.project = panel.getDTO().getNamespace().getProject();
		this.i18n = new RichClientI18NGenerator(project);
		this.securityHelper = new EclipseSecurityHelper(project);
		this.pkTypeName = panel.getAssociation().getDomainObject().getPKAttribute().getJavaType().getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage(panel.getDTO().getNamespace().toString());

		if (!panel.getActions().isEmpty())
			addImports(securityHelper.getSecurityImports());

		panel.getActions().forEach(action -> {
			importPackage("org.eclipse.swt.events");
			importPackage("net.codecadenza.runtime.richclient.eclipse.image");

			if (action.getBoundaryMethod() != null) {
				if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT
						|| action.getType() == ActionType.UPLOAD_IMPORT)
					new EclipseFileHandlingGenerator(this, action, i18n).addImports();
				else if (action.getType() == ActionType.DELETE || action.getType() == ActionType.COPY)
					importClass("org.eclipse.jface.dialogs.MessageDialog");
			}

			if (action.getTargetForm() != null) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_DLG);

				if (action.getType() == ActionType.CREATE || action.getType() == ActionType.UPDATE)
					importClass("org.eclipse.jface.dialogs.Dialog");
			}
		});

		importPackage("net.codecadenza.runtime.richclient.eclipse.search");
		importPackage("net.codecadenza.runtime.richclient.eclipse.widget");
		importPackage("net.codecadenza.runtime.richclient.eclipse.widget.util");
		importClass("org.eclipse.swt.graphics.Image");
		importClass("org.eclipse.swt.SWT");
		importPackage("org.eclipse.swt.widgets");
		importPackage("java.util");

		for (final TableColumnField f : panel.getFormTable().getFields())
			if (f.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN)
				importPackage("net.codecadenza.runtime.richclient.eclipse.image");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class ");
		b.append(panel.getName());
		b.append(" extends AbstractGridResultPanel<" + panel.getDTO().getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addProtectedField("Shell", "parentShell").withFinalModifier().create();
		addPrivateField(pkTypeName, "id").withFinalModifier().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		final FormAction defaultUpdateAction = ActionHelper.getDefaultUpdateAction(panel, securityHelper.isSecurityAdded());
		final FormAction defaultReadonlyAction = ActionHelper.getDefaultReadOnlyAction(panel, securityHelper.isSecurityAdded());
		final boolean addReadonlyParam = panel.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional();
		final FormAction deleteAction = panel.getActions().stream()
				.filter(action -> action.getBoundaryMethod() != null && action.getType() == ActionType.DELETE).findFirst().orElse(null);
		var identifier = "";

		if (addReadonlyParam)
			identifier = panel.getName() + "(Composite parent, Shell parentShell, " + pkTypeName + " id, final boolean readonly)";
		else
			identifier = panel.getName() + "(Composite parent, Shell parentShell, " + pkTypeName + " id)";

		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param parent\n");
		b.append(" * @param parentShell\n");
		b.append(" * @param id\n");

		if (addReadonlyParam)
			b.append(" * @param readonly\n");

		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("super(parent, SWT.NONE);\n\n");

		addDebugLog(b, "Initialize grid panel");

		b.append("\n");
		b.append("this.id = id;\n");
		b.append("this.parentShell = parentShell;\n");

		if (!panel.getActions().isEmpty())
			b.append("\n");

		// Add the toolbar items
		panel.getActions().forEach(action -> {
			final String methodName = createActionMethodName(action);
			final String toolItemName = createActionToolItemName(action);
			final var fb = new StringBuilder();
			boolean isReadOnlyAction = false;

			if (action.getType() == ActionType.READ || action.getType() == ActionType.DOWNLOAD
					|| action.getType() == ActionType.DOWNLOAD_EXPORT)
				isReadOnlyAction = true;

			if (!isReadOnlyAction)
				fb.append("if(!readonly)\n{\n");

			fb.append("final var " + toolItemName + " = new ToolItem(getToolBar(), SWT.PUSH);\n");
			fb.append(toolItemName + ".setImage(");

			if (action.getType() == ActionType.DELETE)
				fb.append("ImageCache.getImage(ImageCache.IMG_DELETE)");
			else if (action.getType() == ActionType.COPY)
				fb.append("ImageCache.getImage(ImageCache.IMG_COPY)");
			else if (action.getType() == ActionType.UPDATE)
				fb.append("ImageCache.getImage(\"edit_data.png\")");
			else if (action.getType() == ActionType.CREATE)
				fb.append("ImageCache.getImage(\"new_data.png\")");
			else if (action.getType() == ActionType.READ)
				fb.append("ImageCache.getImage(\"view_data.png\")");
			else if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT)
				fb.append("ImageCache.getImage(\"download.png\")");
			else if (action.getType() == ActionType.UPLOAD_IMPORT)
				fb.append("ImageCache.getImage(\"upload.png\")");

			fb.append(");\n");
			fb.append(toolItemName + ".setToolTipText(" + i18n.getI18N(panel, action) + ");\n\n");
			fb.append(toolItemName + ".addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");

			if (action.getType() == ActionType.UPLOAD_IMPORT) {
				fb.append("// Disable invocation in order to avoid running two imports concurrently!\n");
				fb.append(toolItemName + ".setEnabled(false);\n\n");
			}

			fb.append(methodName + "();\n");

			if (action.getType() == ActionType.UPLOAD_IMPORT)
				fb.append("\n" + toolItemName + ".setEnabled(true);\n");

			fb.append("}\n");
			fb.append("});\n");

			if (!isReadOnlyAction)
				fb.append("}\n");

			b.append(securityHelper.wrapSecurityCode(action.getRoles(), fb.toString()));
			b.append("\n");
		});

		if (defaultReadonlyAction != null || defaultUpdateAction != null || deleteAction != null) {
			b.append("// Add key listener\n");
			b.append("super.getTableViewer().getTable().addKeyListener(new KeyAdapter()\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void keyReleased(KeyEvent e)\n");
			b.append("{\n");

			if (defaultUpdateAction != null) {
				final var fb = new StringBuilder();
				fb.append("if(!readonly && e.character == SWT.CR)\n");

				if (defaultReadonlyAction != null)
					fb.append("{\n");

				fb.append(createActionMethodName(defaultUpdateAction) + "();\n");

				if (defaultReadonlyAction != null) {
					fb.append("return;\n");
					fb.append("}\n");
				}

				b.append(securityHelper.wrapSecurityCode(defaultUpdateAction.getRoles(), fb.toString()));
			}

			if (defaultReadonlyAction != null) {
				if (defaultUpdateAction != null)
					b.append("\n");

				final var fb = new StringBuilder();
				fb.append("if(e.character == SWT.CR)\n");
				fb.append(createActionMethodName(defaultReadonlyAction) + "();\n");

				b.append(securityHelper.wrapSecurityCode(defaultReadonlyAction.getRoles(), fb.toString()));
			}

			if (deleteAction != null) {
				if (defaultUpdateAction != null || defaultReadonlyAction != null)
					b.append("\n");

				final var fb = new StringBuilder();
				fb.append("if(!readonly && e.character == SWT.DEL)\n");
				fb.append(createActionMethodName(deleteAction) + "();\n");

				b.append(securityHelper.wrapSecurityCode(deleteAction.getRoles(), fb.toString()));
			}

			b.append("}\n");
			b.append("});\n\n");
		}

		if (defaultReadonlyAction != null || defaultUpdateAction != null) {
			b.append("// Add double-click listener\n");
			b.append("super.getTableViewer().addDoubleClickListener(event ->\n");
			b.append("{\n");

			if (defaultUpdateAction != null) {
				final var fb = new StringBuilder();
				fb.append("if(!readonly)\n");
				fb.append("{\n");
				fb.append(createActionMethodName(defaultUpdateAction) + "();\n");
				fb.append("return;\n");
				fb.append("}\n");

				b.append(securityHelper.wrapSecurityCode(defaultUpdateAction.getRoles(), fb.toString()));
			}

			if (defaultReadonlyAction != null) {
				if (defaultUpdateAction != null)
					b.append("\n");

				final String methodInvocation = createActionMethodName(defaultReadonlyAction) + "();\n";

				b.append(securityHelper.wrapSecurityCode(defaultReadonlyAction.getRoles(), methodInvocation));
			}

			b.append("});\n\n");
		}

		// Add context menu items
		panel.getActions().forEach(action -> {
			final var fb = new StringBuilder();
			final String methodName = createActionMethodName(action);

			boolean isReadOnlyAction = false;

			if (action.getType() == ActionType.READ || action.getType() == ActionType.DOWNLOAD
					|| action.getType() == ActionType.DOWNLOAD_EXPORT)
				isReadOnlyAction = true;

			if (!isReadOnlyAction)
				fb.append("if(!readonly)\n{\n");

			fb.append("final var " + action.getName() + "MenuItem = new MenuItem(getPopUpMenu(), SWT.NONE);\n");
			fb.append(action.getName() + "MenuItem.setText(" + i18n.getI18N(panel, action) + ");\n");

			if (action.getType() == ActionType.DELETE)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(ImageCache.IMG_DELETE));\n");
			else if (action.getType() == ActionType.COPY)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(ImageCache.IMG_COPY));\n");
			else if (action.getType() == ActionType.DOWNLOAD || action.getType() == ActionType.DOWNLOAD_EXPORT)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"download.png\"));\n");
			else if (action.getType() == ActionType.UPLOAD_IMPORT)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"upload.png\"));\n");
			else if (action.getType() == ActionType.CREATE)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"new_data.png\"));\n");
			else if (action.getType() == ActionType.UPDATE)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"edit_data.png\"));\n");
			else if (action.getType() == ActionType.READ)
				fb.append(action.getName() + "MenuItem.setImage(ImageCache.getImage(\"view_data.png\"));\n");

			fb.append("\n");
			fb.append(action.getName() + "MenuItem.addSelectionListener(new SelectionAdapter()\n");
			fb.append("{\n");
			fb.append("/* (non-Javadoc)\n");
			fb.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
			fb.append(" */\n");
			fb.append("@Override\n");
			fb.append("public void widgetSelected(SelectionEvent e)\n");
			fb.append("{\n");

			if (action.getType() == ActionType.UPLOAD_IMPORT) {
				fb.append("// Disable invocation in order to avoid running two imports concurrently!\n");
				fb.append(action.getName() + "MenuItem.setEnabled(false);\n\n");
			}

			fb.append(methodName + "();\n");

			if (action.getType() == ActionType.UPLOAD_IMPORT)
				fb.append("\n" + action.getName() + "MenuItem.setEnabled(true);\n");

			fb.append("}\n");
			fb.append("});\n");

			if (!isReadOnlyAction)
				fb.append("}\n");

			b.append(securityHelper.wrapSecurityCode(action.getRoles(), fb.toString()));
			b.append("\n");
		});

		addDebugLog(b, "Grid panel initialization finished");

		b.append("}\n\n");

		addConstructor(identifier, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		StringBuilder b;
		int colIndex = 0;

		// Add private methods for all form actions
		for (final FormAction action : panel.getActions()) {
			final String actionMethodName = createActionMethodName(action);
			b = new StringBuilder();

			if (action.getBoundaryMethod() != null) {
				final DTOBeanAttribute pkAttribute = action.getPanel().getDTO().getPKAttribute();
				final String getter = pkAttribute.getGetterName();
				final String typeName = pkAttribute.getDomainAttribute().getJavaType().getName();
				final BoundaryMethod boundaryMethod = action.getBoundaryMethod();
				final var declarationGenerator = new ServiceDeclarationGenerator(this, boundaryMethod, b);

				if (action.getType() == ActionType.DELETE) {
					b.append("/**\n");
					b.append(" * Delete selected " + panel.getDTO().getDomainObject().getLabel() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");
					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");
					b.append("final boolean ok = MessageDialog.openQuestion(parentShell, ");
					b.append(i18n.getI18NMessage("msg_title_delete", "Delete object"));
					b.append(", " + i18n.getI18NMessage("msg_conf_delete", "Do you really want to delete selected object?") + ");\n\n");
					b.append("if(!ok)\n");
					b.append("return;\n\n");
					b.append("final " + typeName + " selectedObjectId = getSelection()." + getter + ";\n");

					declarationGenerator.addLocalVariable();

					b.append("\n");
					b.append("try\n");
					b.append("{\n");
					b.append("parentShell.setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT));\n\n");

					addDebugLog(b, "Delete selected object with id '{}'", "selectedObjectId");

					b.append("\n");

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("selectedObjectId");

					b.append("refreshData();\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					addErrorLog(b, "Error while deleting selected object!", "ex");

					b.append("\n");
					b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_delete", "Delete object") + ", ");
					b.append(i18n.getI18NMessage("msg_err_delete", "Could not delete selected object! Message: "));
					b.append(" + ex.getMessage());\n");
					b.append("}\n");
					b.append("finally\n");
					b.append("{\n");
					b.append("parentShell.setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_ARROW));\n");

					declarationGenerator.addCloseStatement();

					b.append("}\n");
					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
				else if (action.getType() == ActionType.COPY) {
					final FormAction updateAction = ActionHelper.getDefaultUpdateAction(panel, securityHelper.isSecurityAdded());
					var userIdParam = "";

					b.append("/**\n");
					b.append(" * Create copy of selected " + panel.getDTO().getDomainObject().getLabel() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");
					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");
					b.append("final boolean ok = MessageDialog.openQuestion(parentShell, ");
					b.append(i18n.getI18NMessage("msg_title_copy", "Create copy of selected object") + ", ");
					b.append(i18n.getI18NMessage("msg_conf_copy", "Do you really want to create a copy of selected object?") + ");\n\n");
					b.append("if(!ok)\n");
					b.append("return;\n\n");
					b.append("final " + typeName + " selectedObjectId = getSelection()." + getter + ";\n");

					declarationGenerator.addLocalVariable();

					b.append("\n");
					b.append("try\n");
					b.append("{\n");
					b.append("parentShell.setCursor(parentShell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));\n\n");

					addDebugLog(b, "Create a copy of the selected object with id '{}'", "selectedObjectId");

					b.append("\n");

					if (updateAction != null)
						b.append("final " + typeName + " newId = ");

					if (securityHelper.isSecurityAdded())
						userIdParam = SECURITY_MANAGER + ".getLogOnDTO()."
								+ project.getApplicationLogOnDTO().getPKAttribute().getGetterName();

					new ServiceInvocationGenerator(boundaryMethod, b).addInvocation("selectedObjectId", userIdParam);

					b.append("\n");

					if (updateAction != null) {
						final var fb = new StringBuilder();
						final String targetFormName = updateAction.getTargetForm().getName();

						fb.append("parentShell.setCursor(parentShell.getDisplay().getSystemCursor(SWT.CURSOR_ARROW));\n\n");
						fb.append("final var dlg = new " + targetFormName + "(parentShell, newId);\n");
						fb.append("dlg.open();\n");

						b.append(securityHelper.wrapSecurityCode(updateAction.getRoles(), fb.toString()));
					}

					b.append("\nrefreshData();\n");
					b.append("}\n");
					b.append("catch (final Exception ex)\n");
					b.append("{\n");

					addErrorLog(b, "Error while creating a copy of the selected object!", "ex");

					b.append("\n");
					b.append("MessageDialog.openError(parentShell, ");
					b.append(i18n.getI18NMessage("msg_title_copy", "Create copy of selected object") + ", ");
					b.append(i18n.getI18NMessage("msg_err_copy", "Could not create copy of selected object! Message: "));
					b.append(" + ex.getMessage());\n");
					b.append("}\n");
					b.append("finally\n");
					b.append("{\n");
					b.append("parentShell.setCursor(parentShell.getDisplay().getSystemCursor(SWT.CURSOR_ARROW));\n");

					declarationGenerator.addCloseStatement();

					b.append("}\n");
					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
				else if (action.getType() == ActionType.DOWNLOAD) {
					b.append("/**\n");
					b.append(" * Download " + boundaryMethod.getDomainAttribute().getLabel() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");
					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");
					b.append("final " + typeName + " id = getSelection()." + getter + ";\n\n");
					b.append(new EclipseFileHandlingGenerator(this, action, i18n).createDownloadFragment("id"));
					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
				else if (action.getType() == ActionType.DOWNLOAD_EXPORT) {
					b.append("/**\n");
					b.append(" * " + action.getDescription() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");

					final var exchangeMethod = (DataExchangeMethod) boundaryMethod.getServiceMethod();
					String invocationParameter = null;

					if (exchangeMethod.getSingleObjectFilterParam() != null) {
						b.append("if(getSelection() == null)\n");
						b.append("return;\n\n");
						b.append("final " + typeName + " id = getSelection()." + getter + ";\n");

						invocationParameter = "id";
					}

					if (exchangeMethod.returnsPath())
						b.append(new EclipseFileHandlingGenerator(this, action, i18n).createDownloadFragment(invocationParameter));

					if (exchangeMethod.returnsContent())
						b.append(new EclipseFileHandlingGenerator(this, action, i18n).createDownloadFragmentForExport());

					if (!exchangeMethod.returnsContent() && !exchangeMethod.returnsPath())
						b.append(new EclipseFileHandlingGenerator(this, action, i18n).createExportInvocationFragment());

					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
				else if (action.getType() == ActionType.UPLOAD_IMPORT) {
					b.append("/**\n");
					b.append(" * " + action.getDescription() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");
					b.append(new EclipseFileHandlingGenerator(this, action, i18n).createUploadFragmentForImport(false));
					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
			}
			else if (action.getTargetForm() != null) {
				final String targetFormName = action.getTargetForm().getName();
				final Form targetForm = action.getTargetForm();

				if (targetForm.getFormType() == FormTypeEnumeration.CREATE) {
					b.append("/**\n");
					b.append(" * Create new " + panel.getDTO().getDomainObject().getLabel() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");
					b.append("final var dlg = new " + targetFormName + "(parentShell);\n");
					b.append("final int returnCode = dlg.open();\n\n");
					b.append("if(returnCode == Dialog.CANCEL)\n");
					b.append("return;\n\n");
					b.append("refreshData();\n");
					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
				else if (targetForm.getFormType() == FormTypeEnumeration.ADD) {
					b.append("/**\n");
					b.append(" * Add new " + panel.getDTO().getDomainObject().getLabel() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");
					b.append("final var dlg = new " + targetFormName + "(parentShell, id);\n");
					b.append("final int returnCode = dlg.open();\n\n");
					b.append("if(returnCode == Dialog.CANCEL)\n");
					b.append("return;\n\n");
					b.append("refreshData();\n");
					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
				else if (targetForm.getFormType() == FormTypeEnumeration.UPDATE) {
					b.append("/**\n");
					b.append(" * Edit " + panel.getDTO().getDomainObject().getLabel() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");
					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");

					final DTOBeanAttribute pkAttribute = action.getPanel().getDTO().getPKAttribute();
					final String getter = pkAttribute.getGetterName();
					final String typeName = pkAttribute.getDomainAttribute().getJavaType().getName();

					b.append("final " + typeName + " selectedObjectId = getSelection()." + getter + ";\n\n");
					b.append("final var dlg = new " + targetFormName + "(parentShell, selectedObjectId);\n");
					b.append("final int returnCode = dlg.open();\n\n");
					b.append("if(returnCode == Dialog.CANCEL)\n");
					b.append("return;\n\n");
					b.append("refreshData();\n");
					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
				else if (targetForm.getFormType() == FormTypeEnumeration.READONLY) {
					b.append("/**\n");
					b.append(" * View existing " + panel.getDTO().getDomainObject().getLabel() + "\n");
					b.append(" */\n");
					b.append(getAnnotationForGeneratedElement());
					b.append("private void " + actionMethodName + "()\n");
					b.append("{\n");
					b.append("if(getSelection() == null)\n");
					b.append("return;\n\n");

					final DTOBeanAttribute pkAttribute = action.getPanel().getDTO().getPKAttribute();
					final String getter = pkAttribute.getGetterName();
					final String typeName = pkAttribute.getDomainAttribute().getJavaType().getName();

					b.append("final " + typeName + " selectedObjectId = getSelection()." + getter + ";\n\n");
					b.append("final var dlg = new " + targetFormName + "(parentShell, selectedObjectId);\n");
					b.append("dlg.open();\n");
					b.append("}\n\n");

					addMethod("void " + actionMethodName + "()", b.toString());
				}
			}
		}

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractGridResultPanel#fetchData()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public Collection<" + panel.getDTO().getName() + "> fetchData()\n");
		b.append("{\n");
		b.append("// Get data from server\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, panel.getBoundaryMethod(), b);
		declarationGenerator.addLocalVariable();

		b.append("\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("try\n");
			b.append("{\n");
		}

		b.append("return ");

		new ServiceInvocationGenerator(panel.getBoundaryMethod(), b).addInvocation("id");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();
		}

		b.append("}\n\n");

		addMethod("Collection<" + panel.getDTO().getName() + "> fetchData()", b.toString());

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> {
			final int value1 = col1.getColIndex();
			final int value2 = col2.getColIndex();

			if (value1 == value2)
				return 0;

			if (value1 > value2)
				return 1;

			return -1;
		});

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractGridResultPanel#");
		b.append("initTableColumns(org.eclipse.swt.widgets.Table)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void initTableColumns(Table table)\n");
		b.append("{\n");

		// Add all visible table columns
		int colCounter = 0;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			colCounter++;

			b.append("final var col" + colCounter + " = new TableColumn(table, SWT.NONE);\n");
			b.append("col" + colCounter + ".setWidth(" + col.getWidth() + ");\n");
			b.append("col" + colCounter + ".setText(" + i18n.getI18N(col) + ");\n");
			b.append("col" + colCounter + ".addListener(SWT.Selection, new ColumnSorter(getTableViewer(), col");
			b.append(colCounter + ", ColumnSortType.");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN)
				b.append("BOOLEAN");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.STRING
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.CHAR
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.UUID_BINARY
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.UUID_STRING)
				b.append("STRING");
			else if (col.hasTemporalType()) {
				if (col.hasDateFormat())
					b.append("DATE");
				else {
					b.append("DATETIME");
				}
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.LONG
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.INTEGER)
				b.append("INTEGER");
			else
				b.append("DECIMAL");

			b.append(", " + (colCounter - 1));

			if (col.hasTemporalType()) {
				if (col.hasDateFormat())
					b.append(", userFormat.getDateFormat()");
				else
					b.append(", userFormat.getDateTimeFormat()");
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.FLOAT
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.DOUBLE
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.BIG_DECIMAL)
				b.append(", userFormat.getDecimalFormat()");

			b.append("));\n\n");
		}

		b.append("}\n\n");

		addMethod("void initTableColumns(Table table)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.");
		b.append("__AbstractGridResultPanel#getColImage(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public Image getColImage(" + panel.getDTO().getName() + " object, int columnIndex)\n");
		b.append("{\n");

		boolean firstCol = true;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			colIndex++;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				if (firstCol)
					firstCol = false;
				else
					b.append("else ");

				b.append("if(columnIndex == " + (colIndex - 1) + ")\n");
				b.append("{\n");

				final String getter = col.getDTOAttribute().getGetterName();
				final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();

				if (addNullCheck) {
					b.append("if(object." + getter + " != null)\n");
					b.append("{\n");
				}

				b.append("if(object." + getter + ")\n");
				b.append("return ImageCache.getImage(ImageCache.IMG_CHECKED);\n");
				b.append("else\n");
				b.append("return ImageCache.getImage(ImageCache.IMG_UNCHECKED);\n");

				if (addNullCheck)
					b.append("}\n");

				b.append("}\n");
			}
		}

		if (!firstCol)
			b.append("\n");

		b.append("return null;\n");
		b.append("}\n\n");

		addMethod("Image getColImage(" + panel.getDTO().getName() + " object, int columnIndex)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.");
		b.append("__AbstractGridResultPanel#getColText(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getColText(" + panel.getDTO().getName() + " object, int columnIndex)\n");
		b.append("{\n");
		b.append("switch(columnIndex)\n");
		b.append("{\n");

		colIndex = 0;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				// We should always return an empty string for boolean cells!
				colIndex++;
				continue;
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

				// Generate translations for all literals
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);
			}

			b.append("case " + colIndex++ + ":\n");

			final String getter = col.getDTOAttribute().getGetterName();
			final DomainAttribute domainAttribute = col.getDTOAttribute().getDomainAttribute();
			final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();

			if (addNullCheck)
				b.append("if(object." + getter + " != null)\n");

			b.append("return " + domainAttribute.convertToString("object." + getter) + ";\n");

			if (addNullCheck)
				b.append("else\nreturn \"\";\n");
		}

		b.append("default:\n");
		b.append("return \"\";\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod("String getColText(" + panel.getDTO().getName() + " object, int columnIndex)", b.toString());

		i18n.save();
	}

	/**
	 * Get the method name for the given action
	 * @param action
	 * @return the default method name
	 */
	private String createActionMethodName(FormAction action) {
		// If the action doesn't start with the default prefix we will just return the name!
		return action.getName().replace(ACTION_PREFIX, "perform");
	}

	/**
	 * Get the tool item name for the given action
	 * @param action
	 * @return the default tool item name
	 */
	private String createActionToolItemName(FormAction action) {
		// If the action doesn't start with the default prefix we will just return the name!
		return ACTION_PREFIX_PATTERN.matcher(action.getName()).replaceFirst(BUTTON_PREFIX);
	}

}
