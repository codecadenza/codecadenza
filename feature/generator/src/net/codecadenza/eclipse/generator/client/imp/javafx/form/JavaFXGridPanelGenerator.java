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
package net.codecadenza.eclipse.generator.client.imp.javafx.form;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.security.JavaFXSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.javafx.util.JavaFXActionGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.util.JavaFXCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for grid panels of a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXGridPanelGenerator extends AbstractJavaSourceGenerator {
	private final FormPanel panel;
	private final Project project;
	private final DTOBean dto;
	private final RichClientI18NGenerator i18n;
	private final JavaFXSecurityHelper securityHelper;
	private final JavaFXActionGenerator actionGenerator;
	private final String pkTypeName;
	private final boolean addReadonlyParameter;

	/**
	 * Constructor
	 * @param panel
	 */
	public JavaFXGridPanelGenerator(FormPanel panel) {
		super(panel.getSourceFile());

		this.panel = panel;
		this.project = panel.getDTO().getNamespace().getProject();
		this.dto = panel.getDTO();
		this.i18n = new RichClientI18NGenerator(project);
		this.securityHelper = new JavaFXSecurityHelper(project);
		this.actionGenerator = new JavaFXActionGenerator(this, panel, securityHelper);
		this.pkTypeName = panel.getAssociation().getDomainObject().getPKAttribute().getJavaType().getName();
		this.addReadonlyParameter = panel.getAssociation() instanceof final OneToManyAssociation otm && otm.isBidirectional();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage(dto.getNamespace().toString());
		importPackage("javafx.collections");
		importPackage("javafx.scene.control");
		importPackage("javafx.beans.property");
		importPackage("javafx.stage");
		importPackage("net.codecadenza.runtime.search.dto");
		importPackage("net.codecadenza.runtime.richclient.javafx.search");
		importPackage("java.util");

		actionGenerator.addImports();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class ");
		b.append(panel.getName());
		b.append(" extends AbstractDataGridPanel<" + dto.getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateField(pkTypeName, "id").create();

		if (addReadonlyParameter)
			addPrivateField(JavaType.BOOL, "readonly").create();

		actionGenerator.addFields();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		final String identifier;

		if (addReadonlyParameter)
			identifier = panel.getName() + "(Window owner, " + pkTypeName + " id, final boolean readonly)";
		else
			identifier = panel.getName() + "(Window owner, " + pkTypeName + " id)";

		// Create the constructor
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param owner\n");
		b.append(" * @param id\n");

		if (addReadonlyParameter)
			b.append(" * @param readonly\n");

		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("super(owner);\n\n");
		b.append("this.id = id;\n");

		if (addReadonlyParameter)
			b.append("this.readonly = readonly;\n");

		b.append("\n");
		b.append("// Initialize grid panel\n");
		b.append("initialize();\n\n");
		b.append("// Add columns to table view\n");
		b.append("tableView.getColumns().addAll(initColumns());\n\n");
		b.append("// Initialize all actions\n");
		b.append("initActions();\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final BoundaryMethod method = panel.getBoundaryMethod();
		final var tableGenerator = new JavaFXCommonDataTableGenerator(this, panel.getFormTable(), dto, i18n);
		var methodSignature = "List<" + dto.getName() + "> fetchData()";

		actionGenerator.addActions(i18n);

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		var b = new StringBuilder();
		b.append("\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#fetchData()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + " throws Exception\n");
		b.append("{\n");
		b.append("// Get data from server\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, method, b);
		declarationGenerator.addLocalVariable();

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\ntry\n");
			b.append("{\n");
		}

		b.append("return ");

		new ServiceInvocationGenerator(method, b).addInvocation("id");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "ObservableList<TableColumn<" + dto.getName() + ", String>> initColumns()";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#initColumns()\n");
		b.append(" */\n");
		b.append(tableGenerator.initColumns());

		addMethod(methodSignature, b.toString());

		methodSignature = "String getCellText(" + dto.getName() + " element, int columnIndex)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.");
		b.append("AbstractDataGridPanel#getCellText(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append(tableGenerator.getCellText());

		addMethod(methodSignature, b.toString());

		// Add the method that implements getLogger()
		addGetLoggerMethod("net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel");

		i18n.save();
	}

}
