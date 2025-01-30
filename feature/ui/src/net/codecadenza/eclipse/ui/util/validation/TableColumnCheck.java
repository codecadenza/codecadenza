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

import java.util.Collection;
import java.util.HashSet;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Utility class that provides a static method that checks the consistency of the table column fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TableColumnCheck {
	private static final String DLG_TITLE = "Table column validation";

	private TableColumnCheck() {

	}

	/**
	 * Check if all fields are valid
	 * @param shell
	 * @param formType
	 * @param colFields
	 * @return true if the validation was successful
	 */
	public static boolean checkTableColumnFields(Shell shell, FormTypeEnumeration formType,
			Collection<TableColumnField> colFields) {
		int idCount = 0;
		int lovReturnCount = 0;
		final var fieldNameSet = new HashSet<String>();
		final var fieldTitleSet = new HashSet<String>();
		final var dtoAttrNameSet = new HashSet<String>();

		for (final TableColumnField colField : colFields) {
			if (colField.isIdentifier())
				idCount++;

			if (colField.getDTOAttribute().isLovReturn()) {
				if (!colField.getDTOAttribute().getDomainAttribute().getJavaType().isString()) {
					MessageDialog.openInformation(shell, DLG_TITLE, "The display value must be of type java.lang.String!");
					return false;
				}

				lovReturnCount++;
			}

			// Check if the field name is unique
			if (fieldNameSet.contains(colField.getDTOAttribute().getSelectToken())) {
				final var msg = "A column with the name '" + colField.getDTOAttribute().getSelectToken() + "' already exists!";

				MessageDialog.openInformation(shell, DLG_TITLE, msg);
				return false;
			}

			if (dtoAttrNameSet.contains(colField.getDTOAttribute().getName())) {
				final var msg = "A DTO attribute with the name '" + colField.getDTOAttribute().getName() + "' already exists!";

				MessageDialog.openInformation(shell, DLG_TITLE, msg);
				return false;
			}

			// Check if the field title is unique
			if (fieldTitleSet.contains(colField.getTitle())) {
				MessageDialog.openInformation(shell, DLG_TITLE, "A column with the title '" + colField.getTitle() + "' already exists!");
				return false;
			}

			fieldNameSet.add(colField.getDTOAttribute().getSelectToken());
			fieldTitleSet.add(colField.getTitle());
			dtoAttrNameSet.add(colField.getDTOAttribute().getName());
		}

		// It is not allowed to define multiple ID fields
		if (idCount > 1) {
			MessageDialog.openInformation(shell, DLG_TITLE, "It is not possible to define more than one ID field!");
			return false;
		}

		// It is not allowed to define multiple list-of-values return fields
		if (formType == FormTypeEnumeration.LOV && lovReturnCount > 1) {
			MessageDialog.openInformation(shell, DLG_TITLE, "It is not possible to define more than one list-of-values return field!");
			return false;
		}

		// It should not be possible to add a return field for standard views
		if ((formType == FormTypeEnumeration.SEARCHABLE_VIEW || formType == FormTypeEnumeration.SIMPLE_VIEW) && lovReturnCount > 0) {
			MessageDialog.openInformation(shell, DLG_TITLE, "It is not possible to define a return field for standard views!");
			return false;
		}

		return true;
	}

}
