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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form.field;

import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for fields that open a form by using a link
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseIntLinkFieldGenerator extends AbstractEclipseFieldGenerator {
	private final EclipseSecurityHelper securityHelper;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseIntLinkFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.securityHelper = new EclipseSecurityHelper(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		formGenerator.importPackage("org.eclipse.swt.events");
		formGenerator.addImports(securityHelper.getSecurityImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("Link", field.getName()).create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getDefaultInitializationFragment()
	 */
	@Override
	public String getDefaultInitializationFragment() {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String getter = field.getDTOAttribute().getModelGetterName();
		final DTOBean listDTO = field.getDTOAttribute().getReferencedDTOBean();
		DTOBeanAttribute attr = listDTO.getDisplayAttribute();

		if (attr == null)
			attr = listDTO.getPKAttribute();

		final DomainAttribute domainAttribute = attr.getDomainAttribute();
		final String checkStatement = getCheckFragment();
		final String attributeGetter = modelObjectName + "." + getter + "." + attr.getModelGetterName();

		if (!checkStatement.isEmpty())
			b.append("\n");

		b.append(checkStatement);
		b.append(field.getName() + ".setText(\"<a>\" + " + domainAttribute.convertToString(attributeGetter) + " + \"</a>\");\n");

		if (!checkStatement.isEmpty())
			b.append("\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment()
	 */
	@Override
	public String getFieldDefinitionFragment() {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();

		b.append(field.getName() + " = new Link(" + panel.getName() + ", SWT.NONE);\n");
		b.append(createCommonFormLinkFragment(field.getName()));

		return b.toString();
	}

}
