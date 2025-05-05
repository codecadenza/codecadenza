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

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
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
public class EclipseElementCollectionEditorGenerator extends AbstractEclipseFieldGenerator {
	private final JavaType elementType;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseElementCollectionEditorGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.elementType = field.getDTOAttribute().getDomainAttribute().getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("net.codecadenza.runtime.richclient.eclipse.widget");

		if (elementType.getNamespace() != null)
			formGenerator.importPackage(elementType.getNamespace().getName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("ElementCollectionEditor<" + elementType.getWrapperTypeName() + ">", field.getName()).create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		return getDefaultInitializationFragment();
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

		b.append(field.getName() + ".setElements(" + modelObjectName + "." + getter + ");\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();
		int fieldCount = 0;
		int columnSpan = 0;

		if (!field.isVisible())
			return "";

		// Calculate the number of visible form fields
		for (final FormField field : field.getPanel().getFields())
			if (field.isVisible())
				fieldCount++;

		final boolean span = (field.isSpanCols() && field.getColIndex() == 1) || fieldCount == 1;

		if (!span && field.getColIndex() == 2)
			fillEmptyColumns();

		// Determine the number of columns that the control will take up
		if (field.isSpanCols() && field.getColIndex() == 1)
			columnSpan = fieldCount > 1 ? 3 : 4;
		else if (fieldCount == 1)
			columnSpan = 4;

		// Add a field label if the panel contains further visible fields
		if (fieldCount > 1) {
			b.append("final var " + getFieldLabelName() + " = new Label(" + panel.getName() + ", SWT.NONE);\n");
			b.append(getFieldLabelName() + ".setText(" + i18n.getI18N(field, true) + ");\n\n");
		}

		b.append(field.getName() + " = new ElementCollectionEditor<>(" + panel.getName() + ", SWT.NONE, ");
		b.append(field.isReadonly());
		b.append(", " + elementType.getWrapperTypeName() + ".class);\n");
		b.append(addToolTipFragment());

		if (columnSpan > 0)
			b.append(field.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, " + columnSpan + ", 1));\n\n");
		else
			b.append(field.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n\n");

		if (!span && field.getColIndex() == 1)
			fillEmptyColumns();

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment()
	 */
	@Override
	protected String getFieldDefinitionFragment() {
		return null;
	}

}
