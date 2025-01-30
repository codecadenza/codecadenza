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
package net.codecadenza.eclipse.ui.util.validation;

import java.util.HashSet;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Utility class for the validation of form fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FormFieldCheck {
	private static final String DLG_TITLE = "Form field validation";

	/**
	 * Prevent instantiation
	 */
	private FormFieldCheck() {

	}

	/**
	 * Perform check
	 * @param shell
	 * @param form
	 * @param checkParentFormFields
	 * @return true if the validation was successful
	 */
	public static boolean checkFormFields(Shell shell, Form form, boolean checkParentFormFields) {
		final var attributeSet = new HashSet<String>();
		final var fieldSet = new HashSet<String>();
		int numberOfParentFields = 0;

		for (final FormField field : form.getAllFormFields()) {
			if (form.getFormType() == FormTypeEnumeration.ADD
					&& field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
				numberOfParentFields++;

			if (fieldSet.contains(field.getName())) {
				MessageDialog.openInformation(shell, DLG_TITLE, "A field with the name '" + field.getName() + "' already exists!");
				return false;
			}

			fieldSet.add(field.getName());

			if (attributeSet.contains(field.getDTOAttribute().getName())) {
				MessageDialog.openInformation(shell, DLG_TITLE,
						"A DTO attribute with the name '" + field.getDTOAttribute().getName() + "' already exists!");
				return false;
			}

			attributeSet.add(field.getDTOAttribute().getName());
		}

		// A form of type 'ADD' needs exactly one 'SELECTION_BY_PARENT_FORM' field!
		if (checkParentFormFields && form.getFormType() == FormTypeEnumeration.ADD && numberOfParentFields != 1) {
			MessageDialog.openInformation(shell, DLG_TITLE, "The form requires exactly one field of type 'SELECTION_BY_PARENT_FORM'!");
			return false;
		}

		return true;
	}

}
