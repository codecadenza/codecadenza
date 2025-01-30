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
package net.codecadenza.eclipse.generator.client.imp.vaadin.form.field;

import net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.vaadin.util.VaadinI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Abstract base class for all Vaadin form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractVaadinFieldGenerator extends AbstractClientFieldGenerator {
	public static final int FIELD_COL_INDEX_SECOND_ROW = 4;
	public static final int LABEL_COL_INDEX_SECOND_ROW = 3;
	private static final String EMTPY_CONTENT = "";

	protected VaadinI18NGenerator i18n;

	/**
	 * @return the field's type name
	 */
	protected abstract String getFieldTypeName();

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	protected AbstractVaadinFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
	}

	/**
	 * @param i18n
	 */
	public void setI18n(VaadinI18NGenerator i18n) {
		this.i18n = i18n;
	}

	/**
	 * @return the column index of the form field label
	 */
	public int getLabelColIndex() {
		final int fieldColIndex = field.getColIndex();

		if (fieldColIndex != 1)
			return LABEL_COL_INDEX_SECOND_ROW;

		return 0;
	}

	/**
	 * @return the column index of the form field
	 */
	public int getFieldColIndex() {
		final int fieldColIndex = field.getColIndex();

		if (fieldColIndex != 1)
			return FIELD_COL_INDEX_SECOND_ROW;

		return 1;
	}

	/**
	 * @return the generated content
	 */
	public String getDefaultValueInitialization() {
		return EMTPY_CONTENT;
	}

	/**
	 * @return the generated content
	 */
	public String getConversionFragment() {
		return EMTPY_CONTENT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField(getFieldTypeName(), field.getName()).create();
	}

	/**
	 * @param hasOneColumn
	 * @return the generated content
	 */
	public String getFieldLayout(boolean hasOneColumn) {
		final var b = new StringBuilder();
		b.append("\n");

		if (field.getColIndex() == 2)
			b.append(fillEmptyColumn());

		b.append(getLayoutName() + ".add(" + field.getName() + ");\n");

		if (field.isSpanCols() && field.getColIndex() == 1)
			b.append(getLayoutName() + ".setColspan(" + field.getName() + ", 2);\n");

		if (!hasOneColumn && field.getColIndex() == 1)
			b.append(fillEmptyColumn());

		b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		b.append(field.getName() + " = new " + getFieldTypeName() + "();\n");
		b.append(field.getName() + ".setId(\"" + field.getName() + "\");\n");
		b.append(field.getName() + ".setLabel(" + i18n.getI18N(field) + ");\n");
		b.append(addToolTipFragment(field.getName()));
		b.append(getFieldLayout(hasOneColumn));

		return b.toString();
	}

	/**
	 * @return the name of the parent component
	 */
	public String getLayoutName() {
		return "fl" + field.getPanel().getName().substring(0, 1).toUpperCase() + field.getPanel().getName().substring(1);
	}

	/**
	 * Create the field binding
	 * @param binderName
	 * @return the generated content
	 */
	public String getBinding(String binderName) {
		final DTOBeanAttribute attr = field.getDTOAttribute();

		final var b = new StringBuilder();
		b.append(binderName + ".forField(" + field.getName() + ")");
		b.append(getConversionFragment());
		b.append(getValidationFragment(false));
		b.append(".bind(");

		if (project.isBoundaryMode()) {
			b.append(attr.getDTOBean().getModelClassName() + attr.getGetterReference() + ", ");

			if (!field.isReadonly())
				b.append(attr.getDTOBean().getModelClassName() + attr.getSetterReference());
			else
				b.append("null");
		}
		else if (attr.getDomainAttribute() != null)
			b.append(createAttributeBinding(attr));
		else
			b.append(createAssociationBinding(attr));

		return b.append(");\n").toString();
	}

	/**
	 * @param fieldName
	 * @return the generated content
	 */
	@SuppressWarnings("unused")
	public String addToolTipFragment(String fieldName) {
		return "";
	}

	/**
	 * Create the item label generator for a given DTO attribute
	 * @param attr
	 * @return the generated content
	 */
	protected String addItemLabelGenerator(DTOBeanAttribute attr) {
		final var b = new StringBuilder();
		final DomainAttribute domainAttribute = attr.getDomainAttribute();

		b.append(field.getName() + ".setItemLabelGenerator(");

		if (domainAttribute.getJavaType().isPrimitive())
			b.append("item -> " + domainAttribute.convertToString("item." + attr.getModelGetterName()));
		else {
			b.append("item -> item." + attr.getModelGetterName() + " != null ? ");
			b.append(domainAttribute.convertToString("item." + attr.getModelGetterName()));
			b.append(" : \"\"");
		}

		b.append(");\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	protected String fillEmptyColumn() {
		final var b = new StringBuilder();

		if (!field.fillEmptyGridColumn())
			return "";

		formGenerator.importPackage("com.vaadin.flow.component.html");

		b.append(getLayoutName() + ".add(new NativeLabel());\n");

		return b.toString();
	}

	/**
	 * Create the binding for a domain attribute
	 * @param attr
	 * @return the generated content
	 */
	private String createAttributeBinding(DTOBeanAttribute attr) {
		final var b = new StringBuilder();

		if (attr.getAssociation() != null) {
			// If the attribute cannot be accessed directly it will be necessary to create lambda expressions for the binding!
			b.append(createGetterBinding(attr));
			b.append(createSetterBinding(attr));
		}
		else {
			b.append(attr.getDTOBean().getModelClassName() + attr.getDomainAttribute().getGetterReference() + ", ");

			if (!field.isReadonly())
				b.append(attr.getDTOBean().getModelClassName() + attr.getDomainAttribute().getSetterReference());
			else
				b.append("null");
		}

		return b.toString();
	}

	/**
	 * Create the binding for a domain association
	 * @param attr
	 * @return the generated content
	 */
	private String createAssociationBinding(DTOBeanAttribute attr) {
		final var b = new StringBuilder();

		if (!attr.getAssociationList().isEmpty()) {
			// If the association cannot be accessed directly it will be necessary to create lambda expressions for the binding!
			b.append(createGetterBinding(attr));
			b.append(createSetterBinding(attr));
		}
		else {
			b.append(attr.getDTOBean().getModelClassName() + attr.getAssociation().getGetterReference() + ", ");

			if (!field.isReadonly())
				b.append(attr.getDTOBean().getModelClassName() + attr.getAssociation().getSetterReference());
			else
				b.append("null");
		}

		return b.toString();
	}

	/**
	 * Create the getter binding for the given DTO attribute
	 * @param attr
	 * @return the generated content
	 */
	private String createGetterBinding(DTOBeanAttribute attr) {
		final String checkFragment = getCheckFragment();
		final var b = new StringBuilder();

		if (!checkFragment.isEmpty()) {
			final JavaType type = attr.getDomainAttribute() != null ? attr.getDomainAttribute().getJavaType() : null;

			b.append("obj ->\n");
			b.append("{\n");
			b.append(checkFragment);
			b.append("return obj." + attr.getModelGetterName() + ";\n\n");

			if (type != null && type.isPrimitive()) {
				if (type.isInteger())
					b.append("return 0;\n");
				else if (type.isLong())
					b.append("return 0L;\n");
				else if (type.isDouble())
					b.append("return 0.0;\n");
				else if (type.isFloat())
					b.append("return 0.0F;\n");
				else if (type.isChar())
					b.append("return ' ';\n");
				else if (type.isBoolean())
					b.append("return false;\n");
			}
			else
				b.append("return null;\n");

			b.append("},\n");
		}
		else
			b.append("obj -> obj." + attr.getModelGetterName() + ", ");

		return b.toString();
	}

	/**
	 * Create the setter binding for the given DTO attribute
	 * @param attr
	 * @return the generated content
	 */
	private String createSetterBinding(DTOBeanAttribute attr) {
		final String checkFragment = getCheckFragment();
		final var b = new StringBuilder();

		if (field.isReadonly()) {
			b.append("null");
			return b.toString();
		}

		if (!checkFragment.isEmpty()) {
			b.append("(obj, value) ->\n");
			b.append("{\n");
			b.append(checkFragment);
			b.append("obj." + attr.getModelSetterName() + "(value);");
			b.append("}\n");
		}
		else
			b.append("(obj, value) -> obj." + attr.getModelSetterName() + "(value)");

		return b.toString();
	}

}
