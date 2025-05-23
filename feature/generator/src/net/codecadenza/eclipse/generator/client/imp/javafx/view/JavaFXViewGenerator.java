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
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.security.JavaFXSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.javafx.util.JavaFXActionGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.util.JavaFXCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.CommonGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for views of a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXViewGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final DTOBean dto;
	private final Project project;
	private final RichClientI18NGenerator i18n;
	private final JavaFXSecurityHelper securityHelper;
	private final JavaFXActionGenerator actionGenerator;
	private final FormPanel panel;

	/**
	 * Constructor
	 * @param form
	 */
	public JavaFXViewGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.dto = form.getDTO();
		this.project = form.getDomainObject().getNamespace().getProject();
		this.securityHelper = new JavaFXSecurityHelper(project);
		this.i18n = new RichClientI18NGenerator(project);
		this.actionGenerator = new JavaFXActionGenerator(this, form, securityHelper);
		this.panel = form.getViewFormPanel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage(form.getDTO().getNamespace().toString());
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
		b.append(form.getName());

		if (form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			b.append(" extends AbstractDataGridView<" + form.getDTO().getName() + ">");
		else
			b.append(" extends AbstractSearchGridView<" + form.getDTO().getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		actionGenerator.addFields();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + form.getName() + "()\n");
		b.append("{\n");
		b.append("super(" + i18n.getI18N(form) + ");\n");
		b.append("}\n\n");

		addConstructor(form.getName() + "()", b.toString());

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Constructor\n");
			b.append(" * @param title\n");
			b.append(" * @param searchObj\n");
			b.append(" * @param savedSearchId\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + form.getName() + "(String title, SearchDTO searchObj, Integer savedSearchId)\n");
			b.append("{\n");
			b.append("super(title, searchObj, savedSearchId);\n");
			b.append("}\n\n");

			addConstructor(form.getName() + "(String title, SearchDTO searchObj, Integer savedSearchId)", b.toString());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();

		actionGenerator.addActions(i18n);

		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW) {
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.Countable#countData()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public long countData()\n");
			b.append("{\n");

			final var declarationGenerator = new ServiceDeclarationGenerator(this, form.getBoundaryMethod(), b);
			declarationGenerator.addLocalVariable();

			b.append("\n");

			if (declarationGenerator.needsCloseStatement()) {
				b.append("try\n");
				b.append("{\n");
			}

			for (final FormPanel formPanel : form.getFormPanels()) {
				b.append("return ");

				new ServiceInvocationGenerator(formPanel.getBoundaryMethod(), b).addInvocation("searchObj");
			}

			if (declarationGenerator.needsCloseStatement()) {
				b.append("}\n");

				declarationGenerator.addCloseStatementInFinallyBlock();
			}

			b.append("}\n\n");

			addMethod("long countData(SearchDTO searchDTO)", b.toString());
		}

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		int colOrder = 0;

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView#initSearchObject()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public SearchDTO initSearchObject()\n");
		b.append("{\n");
		b.append("searchObj = new SearchDTO();\n");
		b.append("searchObj.setMaxResult(1000);\n");
		b.append("searchObj.setExactFilterMatch(true);\n");
		b.append("searchObj.setCaseSensitive(false);\n");
		b.append("searchObj.setCount(true);\n");
		b.append("\n");

		// Add all search input fields
		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			boolean addLine = false;

			b.append("final var field" + col.getColIndex() + " = new SearchFieldDTO(" + colOrder++ + ", ");
			b.append(col.getDTOAttribute().getSelectTokenConstant());
			b.append(", ");
			b.append(i18n.getI18N(col));
			b.append(", SearchFieldDataTypeEnum.");
			b.append(col.getFieldType().name());
			b.append(", ");
			b.append(col.getWidth());
			b.append(");\n");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

				b.append("\nfinal var enumListValues" + col.getColIndex() + " = new HashMap<String, String>();\n");

				for (final EnumLiteral value : javaEnum.getEnumerationValues()) {
					b.append("enumListValues" + col.getColIndex() + ".put(\"" + value.getName() + "\", getTranslation");
					b.append("(" + javaEnum.getName().toUpperCase() + "_" + value.getName().toUpperCase() + "));\n");
				}

				b.append("\n");
				b.append("field" + col.getColIndex() + ".setEnumListValues(enumListValues" + col.getColIndex() + ");\n");

				addLine = true;
			}

			if (col.hasDateFormat()) {
				b.append("field" + col.getColIndex() + ".setDateTimeFormat(false);\n");
				addLine = true;
			}

			if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW && !col.isSearchable()) {
				b.append("field" + col.getColIndex() + ".setType(SearchFieldTypeEnum.NOT_SEARCHABLE);\n");
				addLine = true;
			}

			// It is not supported that we query the database in a generic way if we run in an unmanaged environment!
			if (!project.isJavaSEApplication() && col.getFieldType() == TableColumnFieldTypeEnumeration.STRING
					&& col.getLovForm() != null) {
				final DTOBeanAttribute attr = col.getDTOAttribute();
				final DomainAttribute domainAttr = attr.getDomainAttribute();
				var command = "";
				DomainObject lovDomainObj = null;

				// Find the proper domain object for a list-of-values command!
				if (attr.getAssociation() == null) {
					if (!attr.getDTOBean().getDomainObject().isMappedSuperClass())
						lovDomainObj = attr.getDTOBean().getDomainObject();
				}
				else
					lovDomainObj = attr.getAssociation().getTarget();

				if (lovDomainObj != null) {
					command = "select distinct a." + domainAttr.getName() + " from " + lovDomainObj.getName() + " a where a."
							+ domainAttr.getName() + " like :paramPrefix";

					if (domainAttr.getDomainObject().isMandated()
							&& form.getBoundaryMethod().getDataFetchType() == BoundaryMethodDataFetchType.CLIENT) {
						command += " and " + CommonGenerator.getClientAccessFragment(domainAttr.getDomainObject(), "a") + " = ";

						final DTOBeanAttribute clientPKAttr = project.getApplicationLogOnDTO().getClientPKAttribute();

						if (clientPKAttr.getDomainAttribute().getJavaType().isString())
							command += "'";

						b.append("field" + col.getColIndex() + ".setLovCommand(\"" + command + "\" + ");
						b.append(SECURITY_MANAGER + ".getLogOnDTO()." + clientPKAttr.getGetterName());

						if (clientPKAttr.getDomainAttribute().getJavaType().isString())
							b.append(" + \"'\"");

						b.append(");\n");
					}
					else
						b.append("field" + col.getColIndex() + ".setLovCommand(\"" + command + "\");\n");

					addLine = true;
				}
			}

			if (addLine)
				b.append("\n");

			b.append("searchObj.getSearchFields().add(field" + col.getColIndex() + ");\n\n");
		}

		b.append("return searchObj;\n}\n\n");

		addMethod("SearchDTO initSearchObject()", b.toString());

		var methodSignature = "List<" + form.getDTO().getName() + "> fetchData()";

		final BoundaryMethod method = form.getBoundaryMethod();

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView#fetchData()\n");
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

		new ServiceInvocationGenerator(method, b).addInvocation("searchObj");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String getCellText(" + dto.getName() + " element, int columnIndex)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.search.");
		b.append("AbstractDataGridView#getCellText(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append(new JavaFXCommonDataTableGenerator(this, panel.getFormTable(), dto, i18n).getCellText());

		addMethod(methodSignature, b.toString());

		// Add the method that implements getLogger()
		addGetLoggerMethod("net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView");

		i18n.save();
	}

}
