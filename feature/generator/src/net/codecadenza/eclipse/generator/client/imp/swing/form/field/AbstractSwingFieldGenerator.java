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
package net.codecadenza.eclipse.generator.client.imp.swing.form.field;

import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.LOV_PREFIX;

import net.codecadenza.eclipse.generator.client.common.field.AbstractRichClientFieldGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldComparator;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.util.ECollections;

/**
 * <p>
 * Abstract base class for all Swing form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractSwingFieldGenerator extends AbstractRichClientFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	protected AbstractSwingFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);
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
		final String fieldNameWithoutPrefix = field.getName().substring(3);
		final FormPanel panel = field.getPanel();
		final var gbcLabel = "gbcLbl" + fieldNameWithoutPrefix;
		final var gbcField = "gbcField" + fieldNameWithoutPrefix;
		final var scrollPane = "scrollPane" + fieldNameWithoutPrefix;
		final var labelName = "lblField" + fieldNameWithoutPrefix;
		boolean multiLine = false;
		int labelColIndex = 0;
		int fieldColIndex = 0;
		final int realRowIndex = getRealRowIndex();

		if (field.getColIndex() == 1) {
			labelColIndex = 0;
			fieldColIndex = 1;
		}
		else {
			labelColIndex = 2;
			fieldColIndex = 3;
		}

		if (field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT
				|| field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			multiLine = true;

		if (field.isAddFormLinkToLabel()) {
			b.append("final var " + labelName + " = new JXHyperlink();\n");
			b.append(labelName + ".setText(" + i18n.getI18N(field, true) + ");\n");
			b.append(createCommonFormLinkFragment(labelName));
		}
		else
			b.append("final var " + labelName + " = new JLabel(" + i18n.getI18N(field, true) + ");\n");

		if (field.isMandatory() && !field.isReadonly())
			b.append(labelName + ".setForeground(Color.BLUE);\n\n");
		else if (!field.isAddFormLinkToLabel())
			b.append("\n");

		b.append("final var " + gbcLabel + " = new GridBagConstraints();\n");
		b.append(gbcLabel + ".anchor = GridBagConstraints.BASELINE_LEADING;\n");
		b.append(gbcLabel + ".insets = new Insets(5, 5, 5, 5);\n");
		b.append(gbcLabel + ".gridx = " + labelColIndex + ";\n");
		b.append(gbcLabel + ".gridy = " + realRowIndex + ";\n\n");
		b.append(panel.getName() + ".add(" + labelName + ", " + gbcLabel + ");\n\n");

		if (field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT) {
			b.append(field.getName() + " = new JTextPane();\n");

			if (field.isReadonly())
				b.append(field.getName() + ".setEditable(false);\n");
		}
		else if (field.getFieldType() == FormFieldTypeEnumeration.ENUM_COMBOBOX) {
			if (field.isReadonly())
				b.append(field.getName() + " = new JTextField();\n");
			else
				b.append(field.getName() + " = new JComboBox<>();\n");

			if (field.isReadonly())
				b.append(field.getName() + ".setEditable(false);\n");
		}
		else if (field.getFieldType() == FormFieldTypeEnumeration.WEB_LINK
				|| field.getFieldType() == FormFieldTypeEnumeration.MAIL_LINK
				|| field.getFieldType() == FormFieldTypeEnumeration.FORM_LINK)
			b.append(field.getName() + " = new JXHyperlink();\n");
		else if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX)
			b.append(getComboBoxInitFragment());
		else if (field.getFieldType() == FormFieldTypeEnumeration.LABEL
				|| field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			b.append(field.getName() + " = new JLabel();\n");
		else if (field.getFieldType() == FormFieldTypeEnumeration.LOV)
			b.append(getLOVInitFragment());
		else if (field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT)
			b.append(getProposalFieldInitFragment());
		else if (field.getFieldType() == FormFieldTypeEnumeration.CHECKBOX) {
			b.append(field.getName() + " = new JCheckBox();\n");

			if (field.isReadonly())
				b.append(field.getName() + ".setEnabled(false);\n");
		}
		else {
			b.append(field.getName() + " = new JTextField();\n");

			if (field.isReadonly())
				b.append(field.getName() + ".setEditable(false);\n");
		}

		b.append(addToolTipFragment(field.getName()));

		if (multiLine) {
			b.append("\nfinal var " + scrollPane + " = new JScrollPane();\n");
			b.append(scrollPane + ".setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);\n\n");
		}

		b.append("\nfinal var " + gbcField + " = new GridBagConstraints();\n");
		b.append(gbcField + ".anchor = GridBagConstraints.BASELINE;\n");
		b.append(gbcField + ".insets = new Insets(0, 0, 5, 5);\n");

		if (multiLine)
			b.append(gbcField + ".fill = GridBagConstraints.BOTH;\n");
		else
			b.append(gbcField + ".fill = GridBagConstraints.HORIZONTAL;\n");

		b.append(gbcField + ".gridx = " + fieldColIndex + ";\n");
		b.append(gbcField + ".gridy = " + realRowIndex + ";\n");

		if (field.getColIndex() == 1 && field.isSpanCols())
			b.append(gbcField + ".gridwidth = 3;\n");

		b.append("\n");

		if (multiLine) {
			b.append(scrollPane + ".setViewportView(" + field.getName() + ");\n");
			b.append(panel.getName() + ".add(" + scrollPane + ", " + gbcField + ");\n\n");
		}
		else
			b.append(panel.getName() + ".add(" + field.getName() + ", " + gbcField + ");\n\n");

		return b.toString();
	}

	/**
	 * @return the real row index of the field within a GridBagLayout
	 */
	public int getRealRowIndex() {
		// Sort all form fields of this panel
		ECollections.sort(field.getPanel().getFields(), new FormFieldComparator());

		// Calculate the offset as the class GridBagLayout needs 0 for the first row!
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

				// If the previous or next field is also invisible and the row index of both fields is the same we must decrement the real
				// row index!
				if (previousField != null && !previousField.isVisible() && previousField.getRowIndex() == f.getRowIndex())
					realRowIndex--;

				if (nextField != null && !nextField.isVisible() && nextField.getRowIndex() == f.getRowIndex())
					realRowIndex--;
			}
		}

		return realRowIndex;
	}

	/**
	 * @param fieldName
	 * @return the generated content
	 */
	public String createCommonFormLinkFragment(String fieldName) {
		final var b = new StringBuilder();
		final var securityHelper = new SwingSecurityHelper(project);
		final String getter = field.getDTOAttribute().getModelGetterName();
		final DTOBeanAttribute pkAttr = field.getDTOAttribute().getReferencedDTOBean().getPKAttribute();
		final String modelClassName = field.getPanel().getForm().getDTO().getDomainObject().getLowerCaseName();
		final var msgNoSelection = "Cannot open respective detail form because of missing selection value for this field!";
		Form readonlyForm = null;

		for (final Form f : project.getAllFormsOfProject()) {
			if (!f.getDomainObject().equals(field.getDTOAttribute().getReferencedDTOBean().getDomainObject()))
				continue;

			if (f.getFormType() == FormTypeEnumeration.READONLY) {
				readonlyForm = f;
				break;
			}
		}

		b.append("\n");
		b.append(fieldName + ".addActionListener(actionEvent ->\n");
		b.append("{\n");

		if (readonlyForm != null) {
			final var openForm = new StringBuilder();

			if (field.getFieldType() == FormFieldTypeEnumeration.FORM_LINK || field.isReadonly()) {
				// We create a field generator temporarily in order to create a check-fragment
				final var fieldGenerator = new SwingComboboxFieldGenerator(field, formGenerator);
				fieldGenerator.setModelObjectName(field.getPanel().getForm().getDTO().getDomainObject().getLowerCaseName());

				final String checkFragment = fieldGenerator.getCheckFragment();

				openForm.append(checkFragment);

				if (!checkFragment.isEmpty())
					openForm.append("{\n");

				openForm.append("final var dlg = new " + readonlyForm.getName());
				openForm.append("(" + field.getPanel().getForm().getName() + ".this, " + modelClassName);
				openForm.append("." + getter + "." + pkAttr.getModelGetterName() + ");\n");
				openForm.append("dlg.setVisible(true);\n");

				if (!checkFragment.isEmpty()) {
					openForm.append("}\n");
					openForm.append("else\n");
					openForm.append("JOptionPane.showMessageDialog(" + field.getPanel().getForm().getName() + ".this, ");
					openForm.append(i18n.getI18NMessage("msg_info_form_link_no_sel", msgNoSelection) + ", ");
					openForm
							.append(i18n.getI18NMessage("msg_title_form_link", "Open detail form") + ", JOptionPane.INFORMATION_MESSAGE);\n");
				}
			}
			else {
				if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX)
					openForm.append("if(" + field.getName() + ".getSelectedModelObject() == null)\n");

				if (field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT)
					openForm.append("if(" + field.getName() + ".getSelectedElement() == null)\n");
				else if (field.getFieldType() == FormFieldTypeEnumeration.LOV) {
					String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
					lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);

					final JavaType pkType = pkAttr.getDomainAttribute().getJavaType();

					if (pkType.isString())
						openForm.append("if(" + lovIdValue + ".isEmpty())\n");
					else
						openForm.append("if(" + lovIdValue + " == " + pkType.getLocalVariableDefaultValue() + ")\n");
				}

				openForm.append("{\n");
				openForm.append("JOptionPane.showMessageDialog(" + field.getPanel().getForm().getName() + ".this, ");
				openForm.append(i18n.getI18NMessage("msg_info_form_link_no_sel", msgNoSelection));
				openForm.append(", " + i18n.getI18NMessage("msg_title_form_link", "Open detail form"));
				openForm.append(", JOptionPane.INFORMATION_MESSAGE);\n");
				openForm.append("return;\n");
				openForm.append("}\n\n");
				openForm.append("final var dlg = new " + readonlyForm.getName());
				openForm.append("(" + field.getPanel().getForm().getName() + ".this, ");

				if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX)
					openForm.append(field.getName() + ".getSelectedModelObject()" + "." + pkAttr.getModelGetterName());
				else if (field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT)
					openForm.append(field.getName() + ".getSelectedElement()" + "." + pkAttr.getModelGetterName());
				else if (field.getFieldType() == FormFieldTypeEnumeration.LOV) {
					String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
					lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);

					openForm.append(lovIdValue);
				}

				openForm.append(");\n");
				openForm.append("dlg.setVisible(true);\n");
			}

			b.append(securityHelper.wrapSecurityCode(readonlyForm.getRoles(), openForm.toString()));
		}
		else
			b.append("// Appropriate form doesn't exist!\n");

		b.append("});\n\n");

		return b.toString();
	}

	/**
	 * @param fieldName
	 * @return the generated content
	 */
	public String addToolTipFragment(String fieldName) {
		final AbstractDomainAssociation assoc = field.getDTOAttribute().getAssociation();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (attr != null && attr.getUserComment() != null && !attr.getUserComment().isEmpty())
			return fieldName + ".setToolTipText(" + i18n.getI18N(attr) + ");\n";

		if (attr == null && assoc != null && assoc.getUserComment() != null && !assoc.getUserComment().isEmpty())
			return fieldName + ".setToolTipText(" + i18n.getI18N(assoc) + ");\n";

		return "";
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
		b.append("setTitleMessage(" + i18n.getI18NMessage("msg_title_validation_error", "Validation error") + ");\n");
		b.append("setErrorMessage(" + message + ");\n");
		b.append(field.getName() + ".grabFocus();\n");
		b.append("return false;\n");
		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String getProposalFieldInitFragment() {
		final var b = new StringBuilder();

		if (field.isReadonly()) {
			b.append(field.getName() + " = new JTextField();\n");
			b.append(field.getName() + ".setEditable(false);\n");
		}
		else {
			final String modelClassName = field.getDTOAttribute().getReferencedDTOBean().getModelClassName();
			final DTOBean dto = field.getDTOAttribute().getReferencedDTOBean();
			final String formName = field.getPanel().getForm().getName();
			final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dto.getDomainObject());
			final BoundaryMethod method = boundaryBean.getBoundaryMethodByReturnType(dto,
					BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
			DTOBeanAttribute attr = dto.getDisplayAttribute();

			if (attr == null)
				attr = dto.getPKAttribute();

			final var paramFilterLength = attr.getDomainAttribute().getJavaType().isIntegerOrLong() ? ", 1" : "";
			final String getter = "selectedElement." + attr.getModelGetterName();

			b.append("\n");
			b.append(field.getName() + " = new AbstractProposalTextField<>(" + formName + ".this" + paramFilterLength + ")\n");
			b.append("{\n");
			b.append("private static final long serialVersionUID = 1L;\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.");
			b.append("AbstractProposalTextField#getProposalData(java.lang.String)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public Collection<" + modelClassName + "> getProposalData(String filter)\n");
			b.append("{\n");
			b.append("try\n");
			b.append("{\n");
			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));\n\n");
			b.append("return ");

			new ServiceInvocationGenerator(method, dto, b).addInvocation("filter");

			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			formGenerator.addErrorLog(b, "Error while fetching data for proposal text field '" + field.getName() + "'!", "e");

			b.append("\n");
			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
			b.append("JOptionPane.showMessageDialog(" + field.getPanel().getForm().getName() + ".this, ");
			b.append(i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
			b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_dialog_init", "Initialize dialog"));
			b.append(", JOptionPane.WARNING_MESSAGE);\n");
			b.append("return new ArrayList<>();\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
			b.append("}\n");
			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.");
			b.append("AbstractProposalTextField#getDisplayText(java.lang.Object)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public String getDisplayText(" + modelClassName + " selectedElement)\n");
			b.append("{\n");

			if (!field.isMandatory()) {
				b.append("if(selectedElement == null)\n");
				b.append("return \"\";\n\n");
			}

			b.append("return " + attr.getDomainAttribute().convertToString(getter) + ";\n");
			b.append("}\n");
			b.append("};\n\n");
		}

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String getComboBoxInitFragment() {
		final var b = new StringBuilder();

		if (field.isReadonly()) {
			b.append(field.getName() + " = new JTextField();\n");
			b.append(field.getName() + ".setEditable(false);\n");

			return b.toString();
		}

		final DTOBean dto = field.getDTOAttribute().getReferencedDTOBean();
		DTOBeanAttribute dtoAttribute = dto.getDisplayAttribute();

		if (dtoAttribute == null)
			dtoAttribute = dto.getPKAttribute();

		b.append("\n");
		b.append(field.getName() + " = new JDataComboBox<>()\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.JDataComboBox#getItemText(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getItemText(" + dto.getModelClassName() + " element)\n");
		b.append("{\n");

		final String getter = "element." + dtoAttribute.getModelGetterName();
		final JavaType type = dtoAttribute.getDomainAttribute().getJavaType();

		if (!field.isMandatory() && !type.isString()) {
			// Don't display auxiliary numbers that represent ID values of default list items!
			b.append("if(" + getter + " == " + dtoAttribute.getDomainAttribute().getEmptyItemDefaultValue() + ")\nreturn\"\";\n\n");
		}

		b.append("return " + dtoAttribute.getDomainAttribute().convertToString(getter) + ";\n");
		b.append("}\n");
		b.append("};\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String getLOVInitFragment() {
		final var b = new StringBuilder();
		b.append(field.getName() + " = new JTextField();\n");
		b.append(field.getName() + ".setEditable(false);\n");

		if (field.isReadonly())
			return b.toString();

		b.append(field.getName() + ".setBackground(new Color(0xFF, 0xFA, 0xCD));\n");
		b.append(field.getName() + ".setToolTipText(\"Double-click to open selection!\");\n\n");
		b.append(field.getName() + ".addMouseListener(new MouseAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void mouseClicked(MouseEvent e)\n");
		b.append("{\n");
		b.append("if(e.getClickCount() != 2)\n");
		b.append("return;\n\n");

		final DTOBean lovDTO = field.getDTOAttribute().getReferencedDTOBean();
		final Form lovForm = field.getListOfValues();
		final var mto = (ManyToOneAssociation) field.getDTOAttribute().getAssociation();

		b.append("final var lov = new " + lovForm.getName() + "(" + field.getPanel().getForm().getName());
		b.append(".this, " + field.getName() + ".getText(), false, " + mto.isOptional() + ");\n");
		b.append("lov.setModal(true);\n");
		b.append("lov.setVisible(true);\n\n");
		b.append("if(lov.getReturnCode() != JTitleAreaDialog.RETURN_CODE_OK)\n");
		b.append("return;\n\n");

		final DTOBeanAttribute dtoPkAttr = lovDTO.getPKAttribute();
		final JavaType pkType = dtoPkAttr.getDomainAttribute().getJavaType();
		String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
		lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);
		final DTOBeanAttribute displayAttr = lovDTO.getDisplayAttribute();

		if (!pkType.isString()) {
			b.append("if(lov.getIdValue() != null)\n");
			b.append(lovIdValue + " = " + dtoPkAttr.getDomainAttribute().convertFromString("lov.getIdValue()") + ";\n");
			b.append("else\n");
			b.append(lovIdValue + " = " + pkType.getLocalVariableDefaultValue() + ";\n");
		}
		else
			b.append(lovIdValue + " = lov.getIdValue();\n");

		b.append("\n");

		if (displayAttr == null) {
			b.append("if(lov.getIdValue() != null)\n");
			b.append(field.getName() + ".setText(lov.getIdValue());\n");
		}
		else {
			b.append("if(lov.getDisplayValue() != null)\n");
			b.append(field.getName() + ".setText(lov.getDisplayValue());\n");
		}

		b.append("else\n");
		b.append(field.getName() + ".setText(\"\");\n");
		b.append("}\n");
		b.append("});\n\n");

		return b.toString();
	}

}
