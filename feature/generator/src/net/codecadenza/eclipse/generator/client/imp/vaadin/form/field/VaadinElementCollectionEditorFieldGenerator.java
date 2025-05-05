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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for element collection editors
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinElementCollectionEditorFieldGenerator extends AbstractVaadinFieldGenerator {
	private final JavaType elementType;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinElementCollectionEditorFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.elementType = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		if (field.getDTOAttribute().getDomainAttribute().getCollectionType() == CollectionTypeEnumeration.LIST)
			return "ElementCollectionListEditor<" + elementType.getWrapperTypeName() + ">";

		return "ElementCollectionSetEditor<" + elementType.getWrapperTypeName() + ">";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("com.vaadin.flow.component");
		formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.component");

		if (elementType.getNamespace() != null)
			formGenerator.importPackage(elementType.getNamespace().getName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();

		if (field.getDTOAttribute().getDomainAttribute().getCollectionType() == CollectionTypeEnumeration.LIST)
			b.append(field.getName() + " = new ElementCollectionListEditor<>(");
		else
			b.append(field.getName() + " = new ElementCollectionSetEditor<>(");

		b.append(field.isReadonly());
		b.append(", " + elementType.getWrapperTypeName() + ".class, i18n, preferences);\n");
		b.append(field.getName() + ".setHeight(300, Unit.PIXELS);\n");
		b.append(field.getName() + ".setId(\"" + field.getName() + "\");\n");

		int visibleFieldCount = 0;

		// Check if this panel contains other fields. If not, we can omit the field's label and occupy the whole panel!
		for (final FormField f : field.getPanel().getFields())
			if (f.isVisible())
				visibleFieldCount++;

		if (visibleFieldCount == 1) {
			b.append(field.getName() + ".setWidth(600, Unit.PIXELS);\n\n");
			b.append(getLayoutName() + ".add(" + field.getName() + ");\n\n");
		}
		else
			b.append(getFieldLayout(hasOneColumn));

		return b.toString();
	}

}
