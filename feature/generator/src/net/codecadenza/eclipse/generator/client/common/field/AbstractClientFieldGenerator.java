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
package net.codecadenza.eclipse.generator.client.common.field;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Abstract base class for all form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractClientFieldGenerator implements IFieldGenerator {
	protected String modelObjectName;
	protected FormField field;
	protected AbstractJavaSourceGenerator formGenerator;
	protected Project project;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	protected AbstractClientFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		this.field = field;
		this.formGenerator = formGenerator;
		this.project = field.getPanel().getForm().getDomainObject().getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#needsDateTimeFormatter()
	 */
	@Override
	public boolean needsDateTimeFormatter() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#needsDecimalFormatter()
	 */
	@Override
	public boolean needsDecimalFormatter() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#needsDateFormatter()
	 */
	@Override
	public boolean needsDateFormatter() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {

	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#getValidationFragment(boolean)
	 */
	@Override
	public String getValidationFragment(boolean hasTitleArea) {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#getDefaultInitializationFragment()
	 */
	@Override
	public String getDefaultInitializationFragment() {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		return "";
	}

	/**
	 * @return the name of the model object
	 */
	public String getModelObjectName() {
		return modelObjectName;
	}

	/**
	 * @param modelObjectName
	 */
	public void setModelObjectName(String modelObjectName) {
		this.modelObjectName = modelObjectName;
	}

	/**
	 * @return the form field
	 */
	public FormField getField() {
		return field;
	}

	/**
	 * Create the check statement to avoid a NPE if the application tries to access an attribute of an object that doesn't exist.
	 * The framework doesn't support checking of setters as these cases are very unlikely and thus not supported!
	 * @return the generated content
	 */
	public String getCheckFragment() {
		final var b = new StringBuilder();
		final DTOBeanAttribute dtoAttr = field.getDTOAttribute();
		final DomainAttribute domainAttr = dtoAttr.getDomainAttribute();

		if (project.isBoundaryMode()) {
			// In boundary mode we access the attribute value directly via the respective DTO attribute!
			if (domainAttr != null) {
				if (!domainAttr.getJavaType().isPrimitive())
					b.append("if(" + modelObjectName + "." + field.getDTOAttribute().getModelGetterName() + " != null)\n");
			}
			else if (dtoAttr.getAssociation() != null)
				b.append("if(" + modelObjectName + "." + field.getDTOAttribute().getModelGetterName() + " != null)\n");

			return b.toString();
		}

		String attributeValueNullCheck = null;

		if (domainAttr != null && !field.isMandatory() && !domainAttr.getJavaType().isPrimitive()
				&& domainAttr.getDomainAttributeValidator().isNullable())
			attributeValueNullCheck = modelObjectName + "." + dtoAttr.getModelGetterName() + " != null";

		b.append(dtoAttr.getNullCheck(modelObjectName, attributeValueNullCheck));

		return b.toString();
	}

}
