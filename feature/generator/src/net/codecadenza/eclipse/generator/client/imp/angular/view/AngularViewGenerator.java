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
package net.codecadenza.eclipse.generator.client.imp.angular.view;

import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.STRING;

import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for views of an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularViewGenerator extends AbstractTypeScriptSourceGenerator {
	private static final int INITAL_NUMBER_OF_VISIBLE_COLS = 10;

	private final Form form;
	private final FormTypeEnumeration formType;
	private final DomainObject domainObject;
	private final Project project;
	private final AngularI18NGenerator i18n;
	private final AngularCommonDataTableGenerator tableGenerator;

	/**
	 * Constructor
	 * @param form
	 */
	public AngularViewGenerator(Form form) {
		super(form.getTypeScriptSourceFile(), form.getTitle());

		this.form = form;
		this.formType = form.getFormType();
		this.domainObject = form.getDomainObject();
		this.project = domainObject.getNamespace().getProject();
		this.i18n = new AngularI18NGenerator(project);
		this.tableGenerator = new AngularCommonDataTableGenerator(this, form, i18n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importTypes(Stream.of("Component", "OnInit"), "@angular/core");
		importType("SearchInput", "../../common/model/search-input.model");
		importType("AbstractDataGridView", "../../common/components/abstract-data-grid-view/abstract-data-grid-view");

		tableGenerator.addImports();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		var classDeclaration = "export class " + form.getName() + " extends AbstractDataGridView<";
		classDeclaration += form.getDTO().getName() + "> implements OnInit {";

		formatter.addLine("@Component({");
		formatter.increaseIndent();
		formatter.addLine("templateUrl: '../../common/components/abstract-data-grid-view/abstract-data-grid-view.html'");
		formatter.decreaseIndent();
		formatter.addLine("})");
		formatter.addLine(classDeclaration);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addField(STRING, "ID").asConstant("'" + form.getName() + "'").create();
		addField(STRING, "rowKey", "'" + form.getDTO().getPKAttribute().getName() + "'").create();

		addServiceOfSuperclass("ConfirmationService", "confirmationService", "primeng/api");
		addServiceOfSuperclass("MessageService", "messageService", "primeng/api");
		addServiceOfSuperclass("I18NService", "i18n", "../../common/services/i18n.service");
		addServiceOfSuperclass("FormatterService", "formatterService", "../../common/services/formatter.service");
		addServiceOfSuperclass("SavedSearchService", "savedSearchService", "../../common/services/saved-search.service");

		tableGenerator.addFields();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addMethods(AngularContentFormatter formatter) {
		final String invocation = new AngularServiceInvocationGenerator(form.getBoundaryMethod())
				.createInvocation("searchInputBackend");

		formatter.addBlockComment("Initialize the view");
		formatter.addLine("ngOnInit() {");
		formatter.increaseIndent();
		formatter.addLine("this.init(" + form.getName() + ".ID, " + (formType == FormTypeEnumeration.SEARCHABLE_VIEW) + ");");
		formatter.addLine("this.title = " + i18n.getI18N(form) + ";");

		if (tableGenerator.showButtonForCreatingObject())
			formatter.addLine("this.showNewButton = true;");

		if (tableGenerator.showButtonForDataImport())
			formatter.addLine("this.showImportButton = true;");

		tableGenerator.addContextMenuItems();

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		createSearchObjectInitMethod(formatter);

		formatter.addBlockComment("Load the items from the back-end");
		formatter.addLine("loadData() {");
		formatter.increaseIndent();
		formatter.addLine("const searchInputBackend = this.searchInput.convert(this.formatterService.getLocale());");
		formatter.addBlankLine();
		formatter.addLine("return " + invocation + ";");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();

		if (formType == FormTypeEnumeration.SEARCHABLE_VIEW) {
			final BoundaryMethod countMethod = form.getViewFormPanel().getBoundaryMethod();
			final String countInvocation = new AngularServiceInvocationGenerator(countMethod).createInvocation("searchInputBackend");

			formatter.addBlockComment("Perform the count operation");
			formatter.addLine("override countRecords() {");
			formatter.increaseIndent();
			formatter.addLine("const searchInputBackend = this.searchInput.convert(this.formatterService.getLocale());");
			formatter.addBlankLine();
			formatter.addLine("return " + countInvocation + ";");
			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		}

		tableGenerator.createDoubleClickHandler();
		tableGenerator.createActionMethods();

		i18n.save();
	}

	/**
	 * Create the method to initialize the object that defines the columns of the view's data table
	 * @param formatter
	 */
	private void createSearchObjectInitMethod(AngularContentFormatter formatter) {
		final EList<TableColumnField> cols = form.getViewFormPanel().getFormTable().getFields();
		int columnCounter = 0;

		// Sort the columns
		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		formatter.addBlockComment("Define the initial search input object");
		formatter.addLine("initSearchInput(): SearchInput {");
		formatter.increaseIndent();
		formatter.addLine("const searchInput = new SearchInput();");

		for (final TableColumnField col : cols) {
			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM)
				addDependentEnum((JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType());

			if (!col.isVisible())
				continue;

			final boolean addEnumLiterals = formType == FormTypeEnumeration.SEARCHABLE_VIEW
					&& col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM;
			final JavaType type = col.getDTOAttribute().getDomainAttribute().getJavaType();
			final var columnDef = new StringBuilder();
			final var fieldName = "field" + col.getDTOAttribute().getUpperCaseName();
			final boolean resetDateTimeFormatFlag = type.isTemporalType() && col.hasDateFormat();
			boolean hideColumn = false;
			columnCounter++;

			// Limit the number of the initially displayed columns
			if (formType == FormTypeEnumeration.SEARCHABLE_VIEW && columnCounter > INITAL_NUMBER_OF_VISIBLE_COLS)
				hideColumn = true;

			if (resetDateTimeFormatFlag || !col.isSearchable() || addEnumLiterals || hideColumn)
				columnDef.append(formatter.getIndent() + "const " + fieldName + " = searchInput.addSearchField(");
			else
				columnDef.append(formatter.getIndent() + "searchInput.addSearchField(");

			columnDef.append("'" + col.getDTOAttribute().getName() + "', ");
			columnDef.append(i18n.getI18N(col) + ", FieldTypeEnum.");

			if (type.isTemporalType())
				columnDef.append("DATE");
			else if (type.isBoolean())
				columnDef.append("BOOLEAN");
			else if (type.isIntegerOrLong())
				columnDef.append("INTEGER");
			else if (type.isDecimalNumber())
				columnDef.append("DECIMAL");
			else if (type.isString() || type.isChar())
				columnDef.append("STRING");
			else if (type.isUUID()) {
				if (col.getDTOAttribute().getDomainAttribute().isWildcardFilteringSupported())
					columnDef.append("UUID");
				else
					columnDef.append("UUID_BINARY");
			}
			else
				columnDef.append("ENUM");

			columnDef.append(", ");
			columnDef.append(col.getWidth());
			columnDef.append(")");
			columnDef.append(";\n");

			if (addEnumLiterals) {
				final var javaEnum = (JavaEnum) type;

				columnDef.append(formatter.getIndent() + fieldName + ".addSelectionItem('');\n");

				javaEnum.getEnumerationValues().forEach(enumLiteral -> {
					columnDef.append(formatter.getIndent() + fieldName + ".addSelectionItem(");
					columnDef.append(javaEnum.getName() + "[" + javaEnum.getName() + "." + enumLiteral.getName() + "], ");
					columnDef.append(i18n.getI18N(enumLiteral) + ");\n");
				});

				importType(javaEnum.getName(), "../../domain/" + javaEnum.getName().toLowerCase() + ".enum");
			}

			if (resetDateTimeFormatFlag)
				columnDef.append(formatter.getIndent() + fieldName + ".dateTimeFormat = false;\n");

			if (!col.isSearchable())
				columnDef.append(formatter.getIndent() + fieldName + ".displayInDialog = false;\n");

			if (hideColumn)
				columnDef.append(formatter.getIndent() + fieldName + ".displayInTable = false;\n");

			formatter.addBlankLine();
			formatter.addContent(columnDef.toString());
		}

		formatter.addBlankLine();
		formatter.addLine("return searchInput;");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

}
