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

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.security.JavaFXSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.javafx.util.JavaFXCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for list-of-values dialogs of a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXListOfValuesGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final Project project;
	private final DTOBean dto;
	private final RichClientI18NGenerator i18n;
	private final BoundaryMethod method;
	private final FormTable table;

	/**
	 * Constructor
	 * @param form
	 */
	public JavaFXListOfValuesGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.dto = form.getDTO();
		this.project = dto.getNamespace().getProject();
		this.i18n = new RichClientI18NGenerator(project);
		this.method = form.getBoundaryMethod();
		this.table = form.getViewFormPanel().getFormTable();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage("javafx.collections");
		importPackage("javafx.scene.control");
		importPackage("javafx.stage");
		importPackage("javafx.beans.property");
		importPackage("java.util");
		importPackage("net.codecadenza.runtime.richclient.javafx.dialog");
		importPackage("net.codecadenza.runtime.search.dto");
		importPackage(dto.getNamespace().toString());

		if (dto.getDomainObject().isMandated())
			addImports(new JavaFXSecurityHelper(project).getSecurityManagerImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class ");
		b.append(form.getName());
		b.append(" extends AbstractLOVDialog<" + dto.getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param owner\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + form.getName() + "(Window owner)\n");
		b.append("{\n");
		b.append("super(owner, " + i18n.getI18N(form) + ", false);\n");
		b.append("}\n\n");

		addConstructor(form.getName() + "(Window owner)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param owner\n");
		b.append(" * @param enableReset\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + form.getName() + "(Window owner, boolean enableReset)\n");
		b.append("{\n");
		b.append("super(owner, " + i18n.getI18N(form) + ", enableReset);\n");
		b.append("}\n\n");

		addConstructor(form.getName() + "(Window owner, boolean enableReset)", b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var tableGenerator = new JavaFXCommonDataTableGenerator(this, table, dto, i18n);
		var b = new StringBuilder();

		// Sort all table columns
		final EList<TableColumnField> cols = table.getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		var methodSignature = "ObservableList<TableColumn<" + dto.getName() + ", String>> initColumns()";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractLOVDialog#initColumns()\n");
		b.append(" */\n");
		b.append(tableGenerator.initColumns());

		addMethod(methodSignature, b.toString());

		methodSignature = "String getCellText(" + dto.getName() + " element, int columnIndex)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.");
		b.append("AbstractLOVDialog#getCellText(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append(tableGenerator.getCellText());

		addMethod(methodSignature, b.toString());

		methodSignature = "Collection<" + form.getDTO().getName() + "> fetchData()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractLOVDialog#fetchData(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public List<" + dto.getName() + "> fetchData(String filter) throws Exception\n");
		b.append("{\n");
		b.append("// Get data from server\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, method, b);
		declarationGenerator.addLocalVariable();

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\ntry\n");
			b.append("{\n");
		}

		b.append("return ");

		new ServiceInvocationGenerator(method, dto, b).addInvocation("filter");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		// Add the method that implements getLogger()
		addGetLoggerMethod("net.codecadenza.runtime.richclient.javafx.dialog.AbstractLOVDialog");

		i18n.save();
	}

}
