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

import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;

import net.codecadenza.eclipse.generator.client.common.field.AbstractRichClientFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.security.JavaFXSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldComparator;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import org.eclipse.emf.common.util.ECollections;

/**
 * <p>
 * Abstract base class for all JavaFX form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractJavaFXFieldGenerator extends AbstractRichClientFieldGenerator {
	protected JavaFXSecurityHelper securityHelper;
	protected Form form;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	protected AbstractJavaFXFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.form = field.getPanel().getForm();
		this.securityHelper = new JavaFXSecurityHelper(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("javafx.scene.control");
		formGenerator.importPackage("javafx.geometry");
		formGenerator.importStatic("javafx.scene.layout.Region");

		if (needsDecimalFormatter() || needsDateFormatter() || needsDateTimeFormatter()) {
			formGenerator.importPackage("net.codecadenza.runtime.richclient.format");

			if (needsDateFormatter() || needsDateTimeFormatter())
				formGenerator.importPackage(PACK_JAVA_TIME_FORMAT);
			else
				formGenerator.importPackage("java.text");
		}

		if (getTargetFormForLink() != null)
			formGenerator.addImports(new JavaFXSecurityHelper(project).getSecurityImports());
	}

	/**
	 * @return the real row index of the field within a grid layout
	 */
	private int getRealRowIndex() {
		// Sort all form fields of this panel
		ECollections.sort(field.getPanel().getFields(), new FormFieldComparator());

		// Calculate the offset as a JavaFX GridPane needs 0 for the first row!
		final int offset = field.getPanel().getFields().get(0).getRowIndex();
		int realRowIndex = field.getRowIndex() - offset;
		FormField nextField = null;
		FormField previousField = null;

		for (final FormField f : field.getPanel().getFields()) {
			if (f.equals(field))
				return realRowIndex;

			if (f.isVisible())
				continue;

			if (f.isSpanCols())
				realRowIndex--;
			else {
				nextField = null;
				previousField = null;

				final int fieldIndex = field.getPanel().getFields().indexOf(f);

				if (fieldIndex > 0)
					previousField = field.getPanel().getFields().get(fieldIndex - 1);

				if ((fieldIndex + 1) < field.getPanel().getFields().size())
					nextField = field.getPanel().getFields().get(fieldIndex + 1);

				// If either the previous or the next field is also invisible and the row index of both fields is the same we must
				// decrement the real row index!
				if (previousField != null && !previousField.isVisible() && previousField.getRowIndex() == f.getRowIndex())
					realRowIndex--;

				if (nextField != null && !nextField.isVisible() && nextField.getRowIndex() == f.getRowIndex())
					realRowIndex--;
			}
		}

		return realRowIndex;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractRichClientFieldGenerator#
	 * getFieldValidationMessageFragment(java.lang.String, boolean)
	 */
	@Override
	protected String getFieldValidationMessageFragment(String message, boolean hasTitleArea) {
		final var b = new StringBuilder();
		b.append("{\n");
		b.append("final String " + FIELD_LABEL_VALIDATION + " = " + i18n.getI18N(field) + ";\n");

		if (hasTitleArea) {
			b.append("setTitleMessage(" + i18n.getI18NMessage("msg_title_validation_error", "Validation error") + ");\n");
			b.append("setErrorMessage(" + message + ");\n");
		}
		else {
			b.append("final String title = " + i18n.getI18NMessage("msg_title_validation_error", "Validation error") + ";\n");
			b.append("final String message = " + message + ";\n\n");
			b.append("DialogUtil.openWarningDialog(this, title, message);\n");
		}

		b.append(field.getName() + ".requestFocus();\n");
		b.append("return false;\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return a read-only form if a link can be added and an appropriate form exists
	 */
	public Form getTargetFormForLink() {
		if (!field.isAddFormLinkToLabel())
			return null;

		// The link must not be added if the field represents a combobox that is read-only!
		if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX && field.isReadonly())
			return null;

		// Search for a suitable target form
		for (final Form f : project.getAllFormsOfProject()) {
			if (!f.getDomainObject().equals(field.getDTOAttribute().getReferencedDTOBean().getDomainObject()))
				continue;

			if (f.getFormType() == FormTypeEnumeration.READONLY)
				return f;
		}

		// We won't add a hyperlink if no appropriate target form has been found!
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();
		final int realRowIndex = getRealRowIndex();
		final Form readonlyForm = getTargetFormForLink();
		int labelColIndex = 0;
		int fieldColIndex = 0;

		b.append(addToolTipFragment());
		b.append("\n");

		if (field.getColIndex() == 1) {
			labelColIndex = 0;
			fieldColIndex = 1;
		}
		else {
			labelColIndex = 2;
			fieldColIndex = 3;
		}

		if (readonlyForm != null) {
			final var linkName = field.getName() + "Link";

			b.append("final var " + linkName + " = new Hyperlink();\n");
			b.append(linkName + ".setText(" + i18n.getI18N(field, true) + ");\n");
			b.append(linkName + ".setOnAction(event ->\n");
			b.append("{\n");

			final DTOBeanAttribute pkAttr = field.getDTOAttribute().getReferencedDTOBean().getPKAttribute();
			final var openForm = new StringBuilder();

			if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX)
				openForm.append("if(" + field.getName() + ".getValue() == null)\n");
			else
				openForm.append("if(" + field.getName() + ".getSelectedItem() == null)\n");

			final var messageText = "Cannot open respective detail form because of missing selection value for this field!";
			final String title = i18n.getI18NMessage("msg_title_form_link", "Open detail form");

			openForm.append("{\n");
			openForm.append("DialogUtil.openWarningDialog(this, " + title + ", ");
			openForm.append(i18n.getI18NMessage("msg_info_form_link_no_sel", messageText) + ");\n");
			openForm.append("return;\n");
			openForm.append("}\n\n");
			openForm.append("new " + readonlyForm.getName());
			openForm.append("(this, " + field.getName() + ".");

			if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX)
				openForm.append("getValue().");
			else
				openForm.append("getSelectedItem().");

			openForm.append(pkAttr.getModelGetterName());
			openForm.append(").open();\n");

			b.append(securityHelper.wrapSecurityCode(readonlyForm.getRoles(), openForm.toString()));
			b.append("});\n\n");
			b.append(panel.getName() + ".add(" + linkName + ", " + labelColIndex + ", " + realRowIndex + ");\n");
		}
		else if (form.getFormType() == FormTypeEnumeration.READONLY || (!field.isReadonly() && !field.isMandatory())) {
			b.append(panel.getName() + ".add(new Label(" + i18n.getI18N(field, true));
			b.append("), " + labelColIndex + ", " + realRowIndex + ");\n");
		}
		else {
			final var labelName = "lblFor" + field.getName().substring(0, 1).toUpperCase() + field.getName().substring(1);

			b.append("final var " + labelName + " = new Label(" + i18n.getI18N(field, true) + ");\n");

			if (field.isReadonly())
				b.append(labelName + ".setTextFill(javafx.scene.paint.Color.DARKGREY);\n");
			else
				b.append(labelName + ".setTextFill(javafx.scene.paint.Color.BLUE);\n");

			b.append("\n");
			b.append(panel.getName() + ".add(" + labelName + ", " + labelColIndex + ", " + realRowIndex + ");\n");
		}

		b.append(panel.getName() + ".add(" + field.getName() + ", " + fieldColIndex + ", " + realRowIndex);

		if (field.isSpanCols() && fieldColIndex == 1)
			b.append(", 3, 1");

		b.append(");\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	protected String addToolTipFragment() {
		final AbstractDomainAssociation assoc = field.getDTOAttribute().getAssociation();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (attr != null && attr.getUserComment() != null && !attr.getUserComment().isEmpty())
			return field.getName() + ".setTooltip(new Tooltip(" + i18n.getI18N(attr) + "));\n";

		if (attr == null && assoc != null && assoc.getUserComment() != null && !assoc.getUserComment().isEmpty())
			return field.getName() + ".setTooltip(new Tooltip(" + i18n.getI18N(assoc) + "));\n";

		return "";
	}

}
