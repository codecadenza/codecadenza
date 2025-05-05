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
package net.codecadenza.eclipse.generator.client.imp.javafx.form.field;

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
public class JavaFXElementCollectionEditorGenerator extends AbstractJavaFXFieldGenerator {
	private final JavaType elementType;
	private boolean containsOtherFields;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JavaFXElementCollectionEditorGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.elementType = field.getDTOAttribute().getDomainAttribute().getJavaType();

		if (!field.isVisible())
			return;

		// Test if this panel contains other fields
		for (final FormField formField : field.getPanel().getFields()) {
			if (formField.equals(field))
				continue;

			if (!formField.isVisible())
				continue;

			this.containsOtherFields = true;
			break;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("net.codecadenza.runtime.richclient.javafx.control");
		formGenerator.importPackage("javafx.geometry");

		if (elementType.getNamespace() != null)
			formGenerator.importPackage(elementType.getNamespace().getName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("ElementCollectionEditor<" + elementType.getWrapperTypeName() + ">", field.getName()).create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#
	 * getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		return getDefaultInitializationFragment();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#
	 * getDefaultInitializationFragment()
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
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#
	 * getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		boolean addTitle = false;

		// Test if a title should be added
		if (!containsOtherFields) {
			boolean addToTab = false;

			for (final FormPanel panel : form.getFormPanels())
				if (panel.getRowIndex() == field.getPanel().getRowIndex() && !panel.equals(field.getPanel())) {
					addToTab = true;
					break;
				}

			addTitle = !addToTab;
		}

		final var b = new StringBuilder();
		b.append("\n");
		b.append(field.getName() + " = new ElementCollectionEditor<>(");

		if (addTitle)
			b.append(i18n.getI18N(field) + ", ");

		b.append(field.isReadonly());
		b.append(", " + elementType.getWrapperTypeName() + ".class);\n");

		if (!containsOtherFields) {
			b.append(addToolTipFragment());
			b.append("\n");
			b.append(field.getPanel().getName() + ".getChildren().add(" + field.getName() + ");\n");
			b.append("VBox.setVgrow(" + field.getName() + ", Priority.ALWAYS);\n\n");
		}
		else
			b.append(super.getFieldDefinitionFragment(hasOneColumn));

		return b.toString();
	}

}
