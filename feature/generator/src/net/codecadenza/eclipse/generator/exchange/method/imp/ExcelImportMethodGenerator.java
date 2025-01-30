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

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_MAPPING_OBJ_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;

import java.util.Optional;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.exchange.method.AbstractImportMethodGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for Microsoft Excel data import methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExcelImportMethodGenerator extends AbstractImportMethodGenerator {
	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	public ExcelImportMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		super(generator, method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator#addImports()
	 */
	@Override
	public void addImports() {
		super.addImports();

		if (generator == null)
			return;

		generator.importPackage("org.apache.poi.ss.usermodel");

		if (!method.isProcessSingleObject() || !method.getRootElement(true).getSubElements().isEmpty())
			generator.importPackage("java.util");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractImportMethodGenerator#createAdditionalMethods()
	 */
	@Override
	public void createAdditionalMethods() {
		final DataExchangeElement rootElement = method.getRootElement(true);
		final ExchangeMappingObject mappingObject = rootElement.getMappingObject();
		String methodSignature;

		super.createAdditionalMethods();

		if (method.isProcessSingleObject())
			methodSignature = mappingObject.getName() + " ";
		else
			methodSignature = "List<" + mappingObject.getName() + "> ";

		methodSignature += PARSER_METHOD_PREFIX + mappingObject.getDomainObject().getName() + PARSER_METHOD_SUFFIX;
		methodSignature += "(Workbook workbook, " + mappingObject.getName() + " type)";

		addMethodToImportSheet(methodSignature, rootElement, !method.isProcessSingleObject());

		for (final DataExchangeElement element : rootElement.getSubElements()) {
			final AbstractDomainAssociation assoc = element.getMappingAttribute().getAssociation();
			boolean processMultipleObjects = false;

			methodSignature = "List<" + element.getMappingObject().getName() + ">" + PARSER_METHOD_PREFIX + assoc.getUpperCaseName()
					+ PARSER_METHOD_SUFFIX + "(";

			if (method.isProcessSingleObject())
				methodSignature += mappingObject.getName() + " " + DEFAULT_PARENT_OBJ_NAME;
			else
				methodSignature += "List<" + mappingObject.getName() + "> " + DEFAULT_PARENT_LIST_NAME;

			if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation)
				processMultipleObjects = true;

			methodSignature += ", Workbook workbook)";

			addMethodToImportSheet(methodSignature, element, processMultipleObjects);
		}
	}

	/**
	 * @param element
	 * @param processMultipleObjects
	 * @return the generated content
	 */
	private String createJoinFragment(DataExchangeElement element, boolean processMultipleObjects) {
		final var b = new StringBuilder();
		final DataExchangeElement rootElement = method.getRootElement(true);
		final boolean isRootElement = element.equals(rootElement);

		if (isRootElement) {
			if (!method.isProcessSingleObject())
				b.append(DATA_IMPORT_LIST + ".add(" + DEFAULT_MAPPING_OBJ_NAME + ");\n");
		}
		else {
			b.append(DATA_IMPORT_LIST + ".add(" + DEFAULT_MAPPING_OBJ_NAME + ");\n\n");

			if (method.isProcessSingleObject()) {
				b.append(DEFAULT_PARENT_OBJ_NAME + ".");

				if (processMultipleObjects)
					b.append(element.getMappingAttribute().getGetterName() + ".add(");
				else
					b.append(element.getMappingAttribute().getSetterName() + "(");

				b.append(DEFAULT_MAPPING_OBJ_NAME + ");\n");
			}
			else {
				final ExchangeMappingAttribute elementMappingAttr = element.getMappingAttribute();
				final ExchangeMappingAttribute pkAttr = rootElement.getMappingObject().getPKAttribute();

				// Search for a join attribute
				final ExchangeMappingAttribute joinAttr = element.getAttributes().stream()
						.filter(attr -> !attr.isDisableExternalMapping() && attr.getMappingAttribute() != null
								&& attr.getMappingAttribute().isJoinAttribute())
						.findFirst().map(DataExchangeAttribute::getMappingAttribute).orElse(null);

				if (joinAttr != null && pkAttr != null) {
					b.append("for(final " + rootElement.getMappingObject().getName() + " ");
					b.append(DEFAULT_LIST_ITEM_NAME + " : " + DEFAULT_PARENT_LIST_NAME + ")\n");
					b.append("{\n");
					b.append("// Check if both values are set properly!\n");

					if (pkAttr.getDomainAttribute().getJavaType().isPrimitive()) {
						b.append("if(" + DEFAULT_LIST_ITEM_NAME + "." + pkAttr.getGetterName() + " != 0 && ");
						b.append(DEFAULT_MAPPING_OBJ_NAME + "." + joinAttr.getGetterName() + " != 0)\n");
					}
					else {
						b.append("if(" + DEFAULT_LIST_ITEM_NAME + "." + pkAttr.getGetterName() + " != null && ");
						b.append(DEFAULT_MAPPING_OBJ_NAME + "." + joinAttr.getGetterName() + " != null)\n");
					}

					b.append("if(" + DEFAULT_LIST_ITEM_NAME + "." + pkAttr.getGetterName());

					if (!pkAttr.getDomainAttribute().getJavaType().isPrimitive())
						b.append(".equals(");
					else
						b.append(" == ");

					b.append(DEFAULT_MAPPING_OBJ_NAME + "." + joinAttr.getGetterName());

					if (!pkAttr.getDomainAttribute().getJavaType().isPrimitive())
						b.append(")");

					b.append(")\n");
					b.append("{\n");
					b.append(DEFAULT_LIST_ITEM_NAME + ".");

					if (processMultipleObjects)
						b.append(elementMappingAttr.getGetterName() + ".add(");
					else
						b.append(elementMappingAttr.getSetterName() + "(");

					b.append(DEFAULT_MAPPING_OBJ_NAME + ");\n");
					b.append("break;\n");
					b.append("}\n");
					b.append("}\n");
				}
			}
		}

		return b.toString();
	}

	/**
	 * @param methodSignature
	 * @param element
	 * @param processMultipleObjects
	 */
	private void addMethodToImportSheet(String methodSignature, DataExchangeElement element, boolean processMultipleObjects) {
		final var b = new StringBuilder();
		final DataExchangeElement rootElement = method.getRootElement(true);
		final boolean isRootElement = element.equals(rootElement);
		final String mappingClassName = element.getMappingObject().getName();
		boolean processSingleRootElement = false;

		b.append("/**\n");
		b.append(" * Parse " + element.getName() + "\n");

		if (!isRootElement) {
			if (method.isProcessSingleObject())
				b.append(" * @param " + DEFAULT_PARENT_OBJ_NAME + "\n");
			else
				b.append(" * @param " + DEFAULT_PARENT_LIST_NAME + "\n");
		}

		b.append(" * @param workbook\n");

		if (isRootElement) {
			b.append(" * @param type has no functional impact. It is only used to ensure a unique method signature!\n");
			b.append(" * @return ");

			if (method.isProcessSingleObject()) {
				b.append("the mapping object to be imported\n");

				processSingleRootElement = true;
			}
			else
				b.append("a list containing all mapping objects\n");
		}
		else
			b.append(" * @return a list containing all mapping objects\n");

		b.append(" */\n");

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		b.append("private " + methodSignature + "\n");
		b.append("{\n");
		b.append("final Sheet sheet = workbook.getSheet(\"" + element.getName() + "\");\n");
		b.append("boolean rowIsEmpty = true;\n");

		if (isRootElement && method.isProcessSingleObject())
			b.append(mappingClassName + " " + DEFAULT_MAPPING_OBJ_NAME + " = null;\n\n");
		else
			b.append("final var " + DATA_IMPORT_LIST + " = new ArrayList<" + mappingClassName + ">();\n\n");

		b.append("Cell cell = null;\n\n");
		b.append("// Iterate over all rows of this sheet\n");
		b.append("for(final Row row : sheet)\n");
		b.append("{\n");
		b.append("// Skip the first row as we assume that it just contains the column header\n");
		b.append("if(row.getRowNum() == 0)\n");
		b.append("continue;\n\n");

		if (!isRootElement || !method.isProcessSingleObject())
			b.append("final var ");

		b.append(DEFAULT_MAPPING_OBJ_NAME + " = new " + mappingClassName + "();\n\n");

		int columnIndex = 0;

		for (final DataExchangeAttribute attr : element.getAttributes()) {
			if (attr.isDisableExternalMapping() || attr.getMappingAttribute() == null) {
				if (attr.getMappingAttribute() == null)
					columnIndex++;

				continue;
			}

			final ExchangeMappingAttribute mappingAttr = attr.getMappingAttribute();
			final JavaType type = mappingAttr.getJavaType();

			if (columnIndex != 0)
				b.append("\n");

			b.append("// Read contents of cell in column \"" + attr.getName() + "\"\n");
			b.append("cell = row.getCell(" + columnIndex + ");\n\n");
			b.append("if(cell != null && cell.getCellType() != CellType.BLANK)\n");
			b.append("{\n");

			if (type.isTemporalType())
				b.append("if(DateUtil.isCellDateFormatted(cell))\n");

			if (type.isCalendar()) {
				b.append("{\n");
				b.append("final var calendar = new java.util.GregorianCalendar();\n");
				b.append("calendar.setTime(cell.getDateCellValue());\n");
			}

			b.append(DEFAULT_MAPPING_OBJ_NAME + "." + mappingAttr.getSetterName() + "(");

			if (type.isString() || type.isChar())
				b.append("cell.getStringCellValue()");
			else if (type.isDouble())
				b.append("cell.getNumericCellValue()");
			else if (type.isUUID())
				b.append("java.util.UUID.fromString(cell.getStringCellValue())");
			else if (type.isBigDecimal())
				b.append("new java.math.BigDecimal(cell.getNumericCellValue())");
			else if (type.isFloat())
				b.append("(float) cell.getNumericCellValue()");
			else if (type.isInteger())
				b.append("(int) cell.getNumericCellValue()");
			else if (type.isLong())
				b.append("(long) cell.getNumericCellValue()");
			else if (type.isBoolean())
				b.append("cell.getBooleanCellValue()");
			else if (type.isDate())
				b.append("cell.getDateCellValue()");
			else if (type.isLocalDate()) {
				b.append(PACK_JAVA_TIME + ".Instant.ofEpochMilli(cell.getDateCellValue().getTime()).atZone(");
				b.append(PACK_JAVA_TIME + ".ZoneId.systemDefault()).toLocalDate()");
			}
			else if (type.isLocalDateTime()) {
				b.append(PACK_JAVA_TIME + ".Instant.ofEpochMilli(cell.getDateCellValue().getTime()).atZone(");
				b.append(PACK_JAVA_TIME + ".ZoneId.systemDefault()).toLocalDateTime()");
			}
			else if (type.isCalendar())
				b.append("calendar");
			else if (type.isEnum())
				b.append(type.getName() + ".valueOf(cell.getStringCellValue())");
			else
				b.append("null");

			b.append(");\n");

			if (type.isTemporalType())
				b.append("\n");

			b.append("rowIsEmpty = false;\n");

			if (type.isCalendar())
				b.append("}\n");

			b.append("}\n");

			// Initialize a mandatory mapping attribute with an empty string if the respective cell is empty!
			if (type.isString() && mappingAttr.getDomainAttribute() != null) {
				final DomainAttribute domainAttribute = mappingAttr.getDomainAttribute();
				final boolean nullable = domainAttribute.getDomainAttributeValidator().isNullable();
				final Optional<Integer> minLength = domainAttribute.getMinFieldLength();
				final boolean hasDefaultValue = mappingAttr.getDefaultValue() != null && !mappingAttr.getDefaultValue().isEmpty();

				if (minLength.isEmpty() && !nullable && !hasDefaultValue) {
					b.append("else\n");
					b.append(DEFAULT_MAPPING_OBJ_NAME + "." + mappingAttr.getSetterName() + "(\"\");\n");
				}
			}

			columnIndex++;
		}

		if (processSingleRootElement) {
			b.append("\n// Exit the loop as soon as at least one cell contains valid data!\n");
			b.append("if(!rowIsEmpty)\n");
			b.append("break;\n");
		}
		else {
			b.append("\n// If every cell is empty we'll jump to the next row!\n");
			b.append("if(rowIsEmpty)\n");
			b.append("continue;\n\n");
			b.append(createJoinFragment(element, processMultipleObjects));
		}

		b.append("}\n");

		if (isRootElement) {
			b.append("\nreturn ");

			if (method.isProcessSingleObject())
				b.append(DEFAULT_MAPPING_OBJ_NAME);
			else
				b.append(DATA_IMPORT_LIST);

			b.append(";\n");
		}
		else
			b.append("\nreturn " + DATA_IMPORT_LIST + ";\n");

		b.append("}\n\n");

		if (generator != null)
			generator.addMethod(methodSignature, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.exchange.method.AbstractImportMethodGenerator#addUnmarshallerInvocation()
	 */
	@Override
	protected String addUnmarshallerInvocation() {
		final var b = new StringBuilder();
		final String rootMappingClassName = method.getRootElement().getMappingObject().getName();
		String mappingObjectName = DEFAULT_MAPPING_OBJ_NAME;

		if (!method.isProcessSingleObject())
			mappingObjectName = DATA_IMPORT_LIST;

		b.append("final Workbook workbook = WorkbookFactory.create(fileInputStream);\n\n");
		b.append("// Parse respective sheets of Microsoft Excel file\n");

		if (method.isProcessSingleObject())
			b.append("final " + rootMappingClassName);
		else
			b.append("final List<" + rootMappingClassName + ">");

		b.append(" " + mappingObjectName + " = ");

		final DataExchangeElement rootElement = method.getRootElement();
		String methodName = PARSER_METHOD_PREFIX + rootElement.getMappingObject().getDomainObject().getName() + PARSER_METHOD_SUFFIX;

		b.append(methodName + "(workbook, new " + rootMappingClassName + "());\n");

		for (final DataExchangeElement element : rootElement.getSubElements()) {
			methodName = PARSER_METHOD_PREFIX + element.getMappingAttribute().getAssociation().getUpperCaseName()
					+ PARSER_METHOD_SUFFIX;

			b.append(methodName + "(" + mappingObjectName + ", workbook);\n");
		}

		b.append("\n");

		return b.toString();
	}

}
