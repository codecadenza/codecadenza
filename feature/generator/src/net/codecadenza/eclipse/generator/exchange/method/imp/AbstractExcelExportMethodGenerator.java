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
package net.codecadenza.eclipse.generator.exchange.method.imp;

import static net.codecadenza.eclipse.model.java.JavaType.BIG_DECIMAL;
import static net.codecadenza.eclipse.model.java.JavaType.CHAR;
import static net.codecadenza.eclipse.model.java.JavaType.DOUBLE;
import static net.codecadenza.eclipse.model.java.JavaType.DOUBLE_OBJ;
import static net.codecadenza.eclipse.model.java.JavaType.FLOAT;
import static net.codecadenza.eclipse.model.java.JavaType.FLOAT_OBJ;
import static net.codecadenza.eclipse.model.java.JavaType.INT;
import static net.codecadenza.eclipse.model.java.JavaType.INTEGER;
import static net.codecadenza.eclipse.model.java.JavaType.LONG;
import static net.codecadenza.eclipse.model.java.JavaType.LONG_OBJ;
import static net.codecadenza.eclipse.model.java.JavaType.STRING;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.util.BasicEList;

/**
 * <p>
 * Abstract base class for Microsoft Excel data export method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractExcelExportMethodGenerator extends AbstractExportMethodGenerator {
	private static final int MAX_STRING_LENGTH_FOR_AUTOSIZE = 200;
	protected static final String DEFAULT_HIDDEN_SHEET_NAME = "hiddenSheet";

	private int hiddenSheetColCounter;
	private int hiddenSheetColOffset;

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	protected AbstractExcelExportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);
	}

	/**
	 * @return the POI workbook class name
	 */
	protected abstract String getWorkbookClassName();

	/**
	 * @return the POI row class name
	 */
	protected abstract String getRowClassName();

	/**
	 * @return the POI cell class name
	 */
	protected abstract String getCellClassName();

	/**
	 * @return the POI sheet class name
	 */
	protected abstract String getSheetClassName();

	/**
	 * @return the POI cell style class name
	 */
	protected abstract String getCellStyleClassName();

	/**
	 * @return the POI font class name
	 */
	protected abstract String getFontClassName();

	/**
	 * @return the POI spreadsheet version
	 */
	protected abstract String getSpreadsheetVersion();

	/**
	 * @return the POI data validation helper class name
	 */
	protected abstract String getDataValidationHelperClassName();

	/**
	 * @return the POI data validation constraint class name
	 */
	protected abstract String getDataValidationConstraintClassName();

	/**
	 * @param cellStyleObjectName
	 * @return the generated content
	 */
	protected abstract String getHeaderColorFragment(String cellStyleObjectName);

	/**
	 * @return true if a drop down arrow for selection lists should be suppressed
	 */
	protected boolean suppressDropDownArrow() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#addImports()
	 */
	@Override
	public void addImports() {
		super.addImports();

		if (generator == null)
			return;

		generator.importPackage("org.apache.poi.ss.usermodel");

		if (method.isPerformValidation())
			generator.importPackage("org.apache.poi.ss");
	}

	/**
	 * @param element
	 * @return the generated content
	 */
	private String addValidationEntryLists(DataExchangeElement element) {
		final var b = new StringBuilder();

		if (!method.isPerformValidation()) {
			b.append("\n");
			return b.toString();
		}

		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping() || attr.getMappingAttribute() == null)
				continue;

			final JavaType type = attr.getMappingAttribute().getJavaType();
			final String selectionListStatement = attr.getMappingAttribute().getSelectionListStatement();

			if (!attr.isVisible())
				continue;

			if (attr.getValueListEntries().isEmpty() && !type.isEnum()
					&& (selectionListStatement == null || selectionListStatement.isEmpty()))
				continue;

			b.append("// Add selection list entries for column \"" + attr.getName() + "\"\n");

			if (type.isEnum()) {
				b.append("for(int i = 0; i < " + type.getName() + ".values().length; i++)\n");
				b.append("{\n");
				b.append(getRowClassName() + " row = " + DEFAULT_HIDDEN_SHEET_NAME + ".getRow(i);\n\n");
				b.append("if(row == null)\n");
				b.append("row = " + DEFAULT_HIDDEN_SHEET_NAME + ".createRow(i);\n\n");
				b.append("final " + getCellClassName() + " cellListEntry = row.createCell(" + hiddenSheetColCounter + ");\n");
				b.append("cellListEntry.setCellValue(" + type.getName() + ".values()[i].name());\n");
				b.append("}\n\n");

				hiddenSheetColCounter++;

				continue;
			}

			if (!attr.getValueListEntries().isEmpty()) {
				b.append("for(int i = 0; i < " + attr.getValueListEntries().size() + "; i++)\n");
				b.append("{\n");
				b.append(getRowClassName() + " row = " + DEFAULT_HIDDEN_SHEET_NAME + ".getRow(i);\n\n");
				b.append("if(row == null)\n");
				b.append("row = " + DEFAULT_HIDDEN_SHEET_NAME + ".createRow(i);\n\n");
				b.append("final " + getCellClassName() + " cellListEntry = row.createCell(" + hiddenSheetColCounter + ");\n\n");

				int entryIndex = 0;

				b.append("switch(i)\n");
				b.append("{\n");

				for (final ValueListEntry entry : attr.getValueListEntries()) {
					b.append("case " + entryIndex++ + ": ");
					b.append("cellListEntry.setCellValue(\"" + entry.getItemText() + "\");\n");
					b.append("break;\n");
				}

				b.append("default:\n");
				b.append("throw new IllegalStateException(\"There is no value list entry for row index \" + i + \"!\");\n");
				b.append("}\n");
				b.append("}\n\n");

				hiddenSheetColCounter++;

				continue;
			}

			if (selectionListStatement != null && !selectionListStatement.isEmpty()) {
				final String repositoryName = createRepositoryName(element.getMappingObject().getDomainObject());
				final String listTypeName = type.getWrapperTypeName();
				final var listName = attr.getMappingAttribute().getName() + "ListEntries";

				if (generator != null && type.getNamespace() != null)
					generator.importPackage(type.getNamespace().toString());

				// The developer is responsible to provide a valid query!
				b.append("final java.util.List<" + listTypeName + "> " + listName + " = " + repositoryName);
				b.append(".getEntityManager().createQuery(\"" + selectionListStatement + "\", ");
				b.append(listTypeName + ".class).getResultList();\n\n");
				b.append("for(int i = 0; i < " + listName + ".size(); i++)\n");
				b.append("{\n");
				b.append(getRowClassName() + " row = " + DEFAULT_HIDDEN_SHEET_NAME + ".getRow(i);\n\n");
				b.append("if(row == null)\n");
				b.append("row = " + DEFAULT_HIDDEN_SHEET_NAME + ".createRow(i);\n\n");
				b.append("final " + getCellClassName() + " cellListEntry = row.createCell(" + hiddenSheetColCounter + ");\n");
				b.append("cellListEntry.setCellValue(");

				if (type.isChar())
					b.append("String.valueOf(");

				b.append(listName + ".get(i)");

				// Microsoft Excel doesn't support cell values of type BigDecimal!
				if (type.isBigDecimal())
					b.append(".doubleValue()");
				else if (type.isUUID())
					b.append(".toString()");
				else if (type.isChar())
					b.append(")");

				b.append(");\n");
				b.append("}\n\n");

				hiddenSheetColCounter++;
			}
		}

		return b.toString();
	}

	/**
	 * @param element
	 * @return the generated content
	 */
	private String addDataValidationFragment(DataExchangeElement element) {
		final var b = new StringBuilder();
		boolean firstSimpleValidationFragment = true;
		int colCounter = 0;

		if (!method.isPerformValidation())
			return b.toString();

		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping() || attr.getMappingAttribute() == null)
				continue;

			final JavaType type = attr.getMappingAttribute().getJavaType();
			final String typeName = type.getName();
			final String selectionListStatement = attr.getMappingAttribute().getSelectionListStatement();
			colCounter++;

			if (!attr.isVisible())
				continue;

			if (firstSimpleValidationFragment) {
				b.append("final var validationHelper = new " + getDataValidationHelperClassName() + "(sheetData);\n");
				firstSimpleValidationFragment = false;
			}

			if (!attr.getValueListEntries().isEmpty() || type.isEnum()
					|| (selectionListStatement != null && !selectionListStatement.isEmpty())) {
				// Add a drop down list
				final String colIndex = String.valueOf((char) (65 + hiddenSheetColOffset));

				b.append("\n// Add drop down list to column \"" + attr.getName() + "\"\n");
				b.append("final Name namedCellCol" + colCounter + " = workbook.createName();\n");
				b.append("namedCellCol" + colCounter + ".setNameName(\"namedRange" + colIndex + "\");\n");
				b.append("namedCellCol" + colCounter + ".setRefersToFormula(\"" + DEFAULT_HIDDEN_SHEET_NAME);
				b.append("!$" + colIndex + "$1:$" + colIndex + "$\" + ");

				if (type.isEnum())
					b.append(typeName + ".values().length");
				else if (!attr.getValueListEntries().isEmpty())
					b.append(attr.getValueListEntries().size());
				else if (selectionListStatement != null && !selectionListStatement.isEmpty())
					b.append(attr.getMappingAttribute().getName() + "ListEntries.size()");

				b.append(");\n\n");
				b.append("final var rangeListCol" + colCounter + " = ");
				b.append("new org.apache.poi.ss.util.CellRangeAddressList(1, SpreadsheetVersion.");
				b.append(getSpreadsheetVersion() + ".getLastRowIndex(), ");
				b.append((colCounter - 1) + ", " + (colCounter - 1) + ");\n\n");
				b.append("final var dvConstraintCol" + colCounter + " = (" + getDataValidationConstraintClassName());
				b.append(") validationHelper.createFormulaListConstraint(\"namedRange" + colIndex + "\");\n");
				b.append("final DataValidation dataValidationCol" + colCounter);
				b.append(" = validationHelper.createValidation(dvConstraintCol" + colCounter);
				b.append(", rangeListCol" + colCounter + ");\n");
				b.append("dataValidationCol" + colCounter + ".setSuppressDropDownArrow(" + suppressDropDownArrow() + ");\n");
				b.append("dataValidationCol" + colCounter);
				b.append(".createErrorBox(\"Input validation\", \"Please select a valid list entry!\");\n");
				b.append("dataValidationCol" + colCounter + ".setShowErrorBox(true);\n");
				b.append("dataValidationCol" + colCounter + ".setEmptyCellAllowed(");

				if (type.isEnum() || !attr.getValueListEntries().isEmpty())
					b.append("false");
				else {
					final AbstractDomainAssociation assoc = attr.getMappingAttribute().getAssociation();

					if (assoc instanceof final ManyToOneAssociation mto && mto.isOptional())
						b.append("true");
					else
						b.append("false");
				}

				b.append(");\n\n");
				b.append("sheetData.addValidationData(dataValidationCol" + colCounter + ");\n");

				hiddenSheetColOffset++;
			}
			else {
				if (attr.getMappingAttribute().getDomainAttribute() == null
						|| attr.getMappingAttribute().getDomainAttribute().getDomainAttributeValidator() == null)
					continue;

				final DomainAttributeValidator validator = attr.getMappingAttribute().getDomainAttribute().getDomainAttributeValidator();

				if (typeName.equals(STRING) || typeName.equals(CHAR)) {
					Integer minLength = validator.getMinLength();
					Integer maxLength = validator.getMaxLength();

					if (typeName.equals(CHAR)) {
						minLength = 1;
						maxLength = 1;
					}
					else {
						if (minLength == null)
							minLength = 0;

						if (maxLength == null)
							maxLength = Integer.MAX_VALUE;
					}

					b.append("\n// Add string length validation of content in column \"" + attr.getName() + "\"\n");
					b.append("final var dvConstraintCol" + colCounter + " = (" + getDataValidationConstraintClassName());
					b.append(") validationHelper.createTextLengthConstraint(");
					b.append(getDataValidationConstraintClassName() + ".OperatorType.BETWEEN, \"");
					b.append(minLength + "\", \"" + maxLength + "\");\n");
					b.append("final DataValidation dataValidationCol" + colCounter);
					b.append(" = validationHelper.createValidation(dvConstraintCol" + colCounter);
					b.append(", new org.apache.poi.ss.util.CellRangeAddressList(1, SpreadsheetVersion.");
					b.append(getSpreadsheetVersion() + ".getLastRowIndex(), ");
					b.append((colCounter - 1) + ", " + (colCounter - 1) + "));\n");
					b.append("dataValidationCol" + colCounter + ".setShowErrorBox(true);\n");
					b.append("dataValidationCol" + colCounter + ".createErrorBox(\"Input validation\", \"Length of string in column \\\"");
					b.append(attr.getName() + "\\\" is out of range (" + minLength + ", " + maxLength + ")!\");\n\n");
					b.append("sheetData.addValidationData(dataValidationCol" + colCounter + ");\n");
				}
				else if (typeName.equals(INT) || typeName.equals(INTEGER) || typeName.equals(FLOAT) || typeName.equals(FLOAT_OBJ)
						|| typeName.equals(LONG) || typeName.equals(LONG_OBJ) || typeName.equals(DOUBLE) || typeName.equals(DOUBLE_OBJ)
						|| typeName.equals(BIG_DECIMAL)) {
					String minValue = validator.getMinValue();
					String maxValue = validator.getMaxValue();
					var errorMessage = "";

					b.append("\n// Add validation of numeric value in column \"" + attr.getName() + "\"\n");
					b.append("final DataValidationConstraint dvConstraintCol" + colCounter + " = validationHelper.");

					if (minValue != null && !minValue.isEmpty() && (maxValue == null || maxValue.isEmpty())) {
						if (typeName.equals(INT) || typeName.equals(INTEGER) || typeName.equals(LONG) || typeName.equals(LONG_OBJ))
							b.append("createIntegerConstraint");
						else
							b.append("createDecimalConstraint");

						b.append("(" + getDataValidationConstraintClassName() + ".OperatorType.GREATER_OR_EQUAL, \"");
						b.append(minValue + "\", null);\n");

						errorMessage = "Cell value in column \\\"" + attr.getName() + "\\\" must be greater or equal than " + minValue + "!";
					}
					else if (maxValue != null && !maxValue.isEmpty() && (minValue == null || minValue.isEmpty())) {
						if (typeName.equals(INT) || typeName.equals(INTEGER) || typeName.equals(LONG) || typeName.equals(LONG_OBJ))
							b.append("createIntegerConstraint");
						else
							b.append("createDecimalConstraint");

						b.append("(" + getDataValidationConstraintClassName() + ".OperatorType.LESS_OR_EQUAL, \"");
						b.append(maxValue + "\", null);\n");

						errorMessage = "Cell value in column \\\"" + attr.getName() + "\\\" must be less or equal than " + maxValue + "!";
					}
					else if (maxValue != null && !maxValue.isEmpty() && minValue != null && !minValue.isEmpty()) {
						if (typeName.equals(INT) || typeName.equals(INTEGER) || typeName.equals(LONG) || typeName.equals(LONG_OBJ))
							b.append("createIntegerConstraint");
						else
							b.append("createDecimalConstraint");

						b.append("(" + getDataValidationConstraintClassName() + ".OperatorType.BETWEEN, \"");
						b.append(minValue + "\", \"" + maxValue + "\");\n");

						errorMessage = "Cell value in column \\\"" + attr.getName() + "\\\" is out of range (" + minValue + ", " + maxValue
								+ ")!";
					}
					else if ((minValue == null || minValue.isEmpty()) && (maxValue == null || maxValue.isEmpty())) {
						if (typeName.equals(INT) || typeName.equals(INTEGER) || typeName.equals(LONG) || typeName.equals(LONG_OBJ)) {
							if (typeName.equals(INT) || typeName.equals(INTEGER)) {
								minValue = String.valueOf(Integer.MIN_VALUE);
								maxValue = String.valueOf(Integer.MAX_VALUE);
							}
							else {
								minValue = String.valueOf(Long.MIN_VALUE);
								maxValue = String.valueOf(Long.MAX_VALUE);
							}

							b.append("createIntegerConstraint(" + getDataValidationConstraintClassName() + ".OperatorType.BETWEEN, \"");
							b.append(minValue + "\", \"" + maxValue + "\");\n");

							errorMessage = "Cell value in column \\\"" + attr.getName() + "\\\" is not an integer!";
						}
						else {
							b.append("createCustomConstraint(\"isNumber(INDIRECT(ADDRESS(ROW(), COLUMN())))\");\n");

							errorMessage = "Cell value in column \\\"" + attr.getName() + "\\\" must be a number!";
						}
					}

					b.append("final DataValidation dataValidationCol" + colCounter);
					b.append(" = validationHelper.createValidation(dvConstraintCol" + colCounter);
					b.append(", new org.apache.poi.ss.util.CellRangeAddressList(1, SpreadsheetVersion.");
					b.append(getSpreadsheetVersion() + ".getLastRowIndex(), ");
					b.append((colCounter - 1) + ", " + (colCounter - 1) + "));\n");
					b.append("dataValidationCol" + colCounter + ".setShowErrorBox(true);\n");
					b.append("dataValidationCol" + colCounter + ".createErrorBox(\"Input validation\", \"" + errorMessage + "\");\n\n");
					b.append("sheetData.addValidationData(dataValidationCol" + colCounter + ");\n");
				}
			}
		}

		b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#createAdditionalMethods()
	 */
	@Override
	public void createAdditionalMethods() {
		super.createAdditionalMethods();

		final DataExchangeElement rootElement = method.getRootElement(true);
		String sourceObjName = rootElement.getMappingObject().getDomainObject().getLowerCaseName();
		var sourceListName = "";
		var methodSignature = "void " + EXPORT_METHOD_PREFIX + rootElement.getMappingObject().getDomainObject().getName() + "(";

		if (method.isProcessSingleObject())
			methodSignature += rootElement.getMappingObject().getName() + " " + sourceObjName;
		else {
			sourceListName = DEFAULT_LIST_NAME;
			sourceObjName = rootElement.getMappingObject().getDomainObject().getLowerCaseName();
			methodSignature += "List<" + rootElement.getMappingObject().getName() + "> " + sourceListName;
		}

		methodSignature += ", " + getWorkbookClassName() + " workbook, " + getCellStyleClassName() + " headerCellStyle)";

		createMethodToExportSheet(methodSignature, sourceObjName, sourceListName, rootElement, !method.isProcessSingleObject());

		for (final DataExchangeElement element : rootElement.getSubElements()) {
			final AbstractDomainAssociation assoc = element.getMappingAttribute().getAssociation();
			boolean processMultipleObjects = false;
			sourceObjName = DEFAULT_PARENT_OBJ_NAME;

			methodSignature = "void " + EXPORT_METHOD_PREFIX + assoc.getUpperCaseName() + "(";

			if (method.isProcessSingleObject())
				methodSignature += rootElement.getMappingObject().getName() + " " + sourceObjName;
			else {
				sourceListName = DEFAULT_PARENT_LIST_NAME;
				methodSignature += "List<" + rootElement.getMappingObject().getName() + "> " + sourceListName;
			}

			if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation)
				processMultipleObjects = true;

			methodSignature += ", " + getWorkbookClassName() + " workbook, " + getCellStyleClassName() + " headerCellStyle)";

			createMethodToExportSheet(methodSignature, sourceObjName, sourceListName, element, processMultipleObjects);
		}
	}

	/**
	 * @param element
	 * @param checkSubElements
	 * @return true if the respective export method needs a reference to a hidden sheet
	 */
	private boolean addHiddenSheet(DataExchangeElement element, boolean checkSubElements) {
		final var elementList = new BasicEList<DataExchangeElement>();

		if (!method.isPerformValidation())
			return false;

		// If the element represents a root element we must check all sub-elements!
		if (element.getParentElement() == null && checkSubElements)
			elementList.addAll(element.getAllElements());
		else
			elementList.add(element);

		for (final DataExchangeElement e : elementList)
			for (final DataExchangeAttribute attr : e.getAttributes()) {
				if (attr.isDisableExternalMapping() || attr.getMappingAttribute() == null)
					continue;

				final JavaType type = attr.getMappingAttribute().getJavaType();
				final String selectionListStatement = attr.getMappingAttribute().getSelectionListStatement();

				if (!attr.isVisible())
					continue;

				if (attr.getValueListEntries().isEmpty() && !type.isEnum()
						&& (selectionListStatement == null || selectionListStatement.isEmpty()))
					continue;

				return true;
			}

		return false;
	}

	/**
	 * @param methodSignature
	 * @param sourceObjectName
	 * @param sourceListName
	 * @param element
	 * @param processMultipleObjects
	 */
	private void createMethodToExportSheet(String methodSignature, String sourceObjectName, String sourceListName,
			DataExchangeElement element, boolean processMultipleObjects) {
		final var b = new StringBuilder();
		boolean dateFormatSet = false;
		boolean dateTimeFormatSet = false;
		boolean textFormatSet = false;
		final DataExchangeElement rootElement = method.getRootElement(true);
		hiddenSheetColOffset = hiddenSheetColCounter;

		b.append("/**\n");
		b.append(" * Export data to " + element.getName() + "\n");

		if (!rootElement.equals(element)) {
			if (!method.isProcessSingleObject())
				b.append(" * @param " + sourceListName + "\n");
			else
				b.append(" * @param " + sourceObjectName + "\n");
		}
		else if (processMultipleObjects)
			b.append(" * @param " + sourceListName + "\n");
		else
			b.append(" * @param " + sourceObjectName + "\n");

		b.append(" * @param workbook\n");
		b.append(" * @param headerCellStyle\n");
		b.append(" */\n");

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("final " + getSheetClassName() + " sheetData = workbook.createSheet(\"" + element.getName() + "\");\n");

		// Check if a hidden sheet containing selection list entries is needed
		if (addHiddenSheet(element, true))
			if (element.equals(rootElement)) {
				b.append("\n// Create hidden sheet that contains selection list items\n");

				// Check if this method needs a reference to the object representing the hidden sheet!
				if (addHiddenSheet(element, false))
					b.append("final " + getSheetClassName() + " " + DEFAULT_HIDDEN_SHEET_NAME + " = ");

				b.append("workbook.createSheet(\"" + DEFAULT_HIDDEN_SHEET_NAME + "\");\n");
				b.append("workbook.setSheetHidden(1, true);\n\n");
			}
			else {
				b.append("final " + getSheetClassName() + " " + DEFAULT_HIDDEN_SHEET_NAME);
				b.append(" = workbook.getSheet(\"" + DEFAULT_HIDDEN_SHEET_NAME + "\");\n\n");
			}

		b.append(addValidationEntryLists(element));
		b.append("int rowCounter = -1;\n");
		b.append("int colCounter = -1;\n\n");
		b.append("// Create header row\n");
		b.append("final " + getRowClassName() + " headerRow = sheetData.createRow(++rowCounter);\n\n");

		int attrCounter = 1;

		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping())
				continue;

			b.append("final " + getCellClassName() + " headerCell" + attrCounter + " = headerRow.createCell(++colCounter);\n");
			b.append("headerCell" + attrCounter + ".setCellValue(\"" + attr.getName() + "\");\n");
			b.append("headerCell" + attrCounter + ".setCellStyle(headerCellStyle);\n\n");

			attrCounter++;
		}

		// Add default styles for cells
		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping())
				continue;

			JavaType type = null;
			ExchangeMappingAttribute mappingAttr = null;

			if (attr.getMappingAttribute() != null) {
				mappingAttr = attr.getMappingAttribute();
				type = mappingAttr.getJavaType();
			}
			else
				type = attr.getDataType();

			if (type.isTemporalType()) {
				TemporalTypeEnumeration temporalType = TemporalTypeEnumeration.TIMESTAMP;
				DomainAttribute domainAttr = null;

				if (mappingAttr != null)
					domainAttr = mappingAttr.getDomainAttribute();

				if (type.isLocalDate() || (domainAttr != null && domainAttr.getTemporalType() == TemporalTypeEnumeration.DATE))
					temporalType = TemporalTypeEnumeration.DATE;

				if (temporalType == TemporalTypeEnumeration.DATE && !dateFormatSet) {
					b.append("final " + getCellStyleClassName() + " dateStyle = workbook.createCellStyle();\n");
					b.append("dateStyle.setDataFormat(workbook.createDataFormat().getFormat(\"dd/mm/yyyy\"));\n\n");

					dateFormatSet = true;
				}

				if (temporalType == TemporalTypeEnumeration.TIMESTAMP && !dateTimeFormatSet) {
					b.append("final " + getCellStyleClassName() + " dateTimeStyle = workbook.createCellStyle();\n");
					b.append("dateTimeStyle.setDataFormat(workbook.createDataFormat().getFormat(\"dd/mm/yyyy hh:mm:ss\"));\n\n");

					dateTimeFormatSet = true;
				}
			}
			else if ((type.isString() || type.isUUID() || type.isChar() || type.isEnum()) && !textFormatSet) {
				b.append("final " + getCellStyleClassName() + " textStyle = workbook.createCellStyle();\n");
				b.append("textStyle.setDataFormat(workbook.createDataFormat().getFormat(\"@\"));\n\n");

				textFormatSet = true;
			}
		}

		if (!rootElement.equals(element)) {
			if (!method.isProcessSingleObject()) {
				b.append("for(final " + rootElement.getMappingObject().getName() + " ");
				b.append(sourceObjectName + " : " + sourceListName + ")\n");

				if (processMultipleObjects) {
					b.append("for(final " + element.getMappingObject().getName() + " " + DEFAULT_LIST_ITEM_NAME);
					b.append(" : " + sourceObjectName + "." + element.getMappingAttribute().getGetterName() + ")\n");

					sourceObjectName = DEFAULT_LIST_ITEM_NAME;
				}
				else
					sourceObjectName = sourceObjectName + "." + element.getMappingAttribute().getGetterName();
			}
			else if (processMultipleObjects) {
				b.append("for(final " + element.getMappingObject().getName() + " " + DEFAULT_LIST_ITEM_NAME);
				b.append(" : " + sourceObjectName + "." + element.getMappingAttribute().getGetterName() + ")\n");

				sourceObjectName = DEFAULT_LIST_ITEM_NAME;
			}
			else
				sourceObjectName = sourceObjectName + "." + element.getMappingAttribute().getGetterName();
		}
		else if (processMultipleObjects)
			b.append("for(final " + element.getMappingObject().getName() + " " + sourceObjectName + " : " + sourceListName + ")\n");

		if (!method.isProcessSingleObject() || processMultipleObjects)
			b.append("{\n");

		b.append("final " + getRowClassName() + " row = sheetData.createRow(++rowCounter);\n");
		b.append("colCounter = -1;\n\n");

		int colCounter = 1;

		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping())
				continue;

			if (colCounter != 1)
				b.append("\n");

			JavaType type = null;
			ExchangeMappingAttribute mappingAttr = null;

			if (attr.getMappingAttribute() != null) {
				mappingAttr = attr.getMappingAttribute();
				type = mappingAttr.getJavaType();
			}
			else
				type = attr.getDataType();

			b.append("// Create cell for column \"" + attr.getName() + "\"\n");
			b.append("final " + getCellClassName() + " cell" + colCounter + " = row.createCell(++colCounter);\n");

			if (type.isBoolean())
				b.append("cell" + colCounter + ".setCellType(CellType.BOOLEAN);\n");
			else if (type.isString() || type.isUUID() || type.isChar() || type.isEnum())
				b.append("cell" + colCounter + ".setCellType(CellType.STRING);\n");
			else
				b.append("cell" + colCounter + ".setCellType(CellType.NUMERIC);\n");

			if (type.isTemporalType()) {
				DomainAttribute domainAttr = null;

				if (mappingAttr != null)
					domainAttr = mappingAttr.getDomainAttribute();

				if (type.isLocalDate() || (domainAttr != null && domainAttr.getTemporalType() == TemporalTypeEnumeration.DATE))
					b.append("cell" + colCounter + ".setCellStyle(dateStyle);\n");
				else
					b.append("cell" + colCounter + ".setCellStyle(dateTimeStyle);\n");
			}
			else if (type.isString() || type.isUUID() || type.isChar() || type.isEnum())
				b.append("cell" + colCounter + ".setCellStyle(textStyle);\n");

			if (mappingAttr != null) {
				if (!type.isPrimitive())
					b.append("\nif(" + sourceObjectName + "." + mappingAttr.getGetterName() + " != null)\n");

				b.append("cell" + colCounter + ".setCellValue(");

				if (type.isLocalDateTime() || type.isLocalDate())
					b.append("java.util.Date.from(");

				b.append(sourceObjectName + "." + mappingAttr.getGetterName());

				if (type.isBigDecimal())
					b.append(".doubleValue()");
				else if (type.isUUID())
					b.append(".toString()");
				else if (type.isEnum())
					b.append(".name()");
				else if (type.isLocalDateTime())
					b.append(".atZone(" + PACK_JAVA_TIME + ".ZoneId.systemDefault()).toInstant())");
				else if (type.isLocalDate())
					b.append(".atStartOfDay().atZone(" + PACK_JAVA_TIME + ".ZoneId.systemDefault()).toInstant())");

				b.append(");\n");

				if (!type.isPrimitive()) {
					b.append("else\n");
					b.append("cell" + colCounter + ".setCellType(CellType.BLANK);\n");
				}
			}

			colCounter++;
		}

		if (!method.isProcessSingleObject() || processMultipleObjects)
			b.append("}\n");

		b.append("\n");
		b.append(addDataValidationFragment(element));

		colCounter = 0;
		boolean hiddenColExists = false;

		// Hide columns that are set to invisible
		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping())
				continue;

			if (!attr.isVisible()) {
				hiddenColExists = true;
				b.append("sheetData.setColumnHidden(" + colCounter + ", true);\n");
			}

			colCounter++;
		}

		if (hiddenColExists)
			b.append("\n");

		colCounter = 0;

		boolean firstCol = true;

		// Calling of method autoSizeColumn() causes severe problems in Eclipse RAP 3.0 applications
		if (!project.hasRAPClient()) {
			// Adjust the column width automatically
			for (final DataExchangeAttribute attr : element.getAttributes()) {
				if (attr.isDisableExternalMapping())
					continue;

				boolean autosize = true;

				if (attr.isVisible() && attr.getMappingAttribute() != null) {
					final DomainAttribute domainAttr = attr.getMappingAttribute().getDomainAttribute();

					if (domainAttr != null && domainAttr.getJavaType().isString()) {
						final DomainAttributeValidator validator = domainAttr.getDomainAttributeValidator();

						// We don't adjust the column width if the content is too long!
						if (validator != null && validator.getMaxLength() != null
								&& validator.getMaxLength() > MAX_STRING_LENGTH_FOR_AUTOSIZE)
							autosize = false;
					}
				}
				else
					autosize = false;

				if (autosize) {
					if (firstCol) {
						b.append("// Adjust the column widths to fit the contents\n");
						firstCol = false;
					}

					b.append("sheetData.autoSizeColumn(" + colCounter + ");\n");
				}

				colCounter++;
			}
		}

		b.append("}\n\n");

		if (generator != null)
			generator.addMethod(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExportMethodGenerator#
	 * addMarshallerInvocation(java.lang.String)
	 */
	@Override
	protected String addMarshallerInvocation(String marshalledObject) {
		final var b = new StringBuilder();
		final var headerStyleObjName = "headerCellStyle";
		hiddenSheetColCounter = 0;

		b.append("final var workbook = new " + getWorkbookClassName() + "();\n\n");
		b.append("final " + getFontClassName() + " headerFont = workbook.createFont();\n");
		b.append("headerFont.setFontHeightInPoints((short) 11);\n");
		b.append("headerFont.setBold(true);\n\n");
		b.append("final " + getCellStyleClassName() + " " + headerStyleObjName + " = workbook.createCellStyle();\n");
		b.append(headerStyleObjName + ".setFont(headerFont);\n");
		b.append(getHeaderColorFragment(headerStyleObjName));
		b.append(headerStyleObjName + ".setFillPattern(FillPatternType.SOLID_FOREGROUND);\n\n");

		// Invoke methods that fills respective sheets!
		final DataExchangeElement rootElement = method.getRootElement(true);
		String methodName = EXPORT_METHOD_PREFIX + rootElement.getMappingObject().getDomainObject().getName();

		b.append("// Fill sheets\n");
		b.append(methodName + "(" + marshalledObject + ", workbook, " + headerStyleObjName + ");\n");

		for (final DataExchangeElement element : rootElement.getSubElements()) {
			methodName = EXPORT_METHOD_PREFIX + element.getMappingAttribute().getAssociation().getUpperCaseName();

			b.append(methodName + "(" + marshalledObject + ", workbook, " + headerStyleObjName + ");\n");
		}

		b.append("\n");
		b.append("try(final var fileOutputStream = new FileOutputStream(outputFile))\n");
		b.append("{\n");
		b.append("workbook.write(fileOutputStream);\n");
		b.append("}\n");

		if (method.returnsPath())
			b.append("\nreturn outputFile.getAbsolutePath();\n");

		return b.toString();
	}

}
