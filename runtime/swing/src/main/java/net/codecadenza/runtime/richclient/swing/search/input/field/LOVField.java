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
package net.codecadenza.runtime.richclient.swing.search.input.field;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.LOV_FIELD_BTN_OPEN_DIALOG;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.LOV_FIELD_MSG_IN_OPERATOR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.LOV_FIELD_MSG_MISSING_INPUT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.LOV_FIELD_MSG_OPERATION_FAILED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.lang.invoke.MethodHandles;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JTextField;
import net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog;
import net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog;
import net.codecadenza.runtime.richclient.swing.search.util.Operators;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Input field for list-of-values
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LOVField extends InputField {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = -2750621062529993563L;

	private final JTextField txtValue;
	private final JButton btnOpenDialog;

	/**
	 * Constructor
	 */
	public LOVField() {
		this.txtValue = new JTextField();
		this.btnOpenDialog = new JButton(getTranslation(LOV_FIELD_BTN_OPEN_DIALOG));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#doAddTo(java.awt.Container, int, int,
	 * java.awt.GridBagConstraints)
	 */
	@Override
	public void doAddTo(Container grid, int row, int col, GridBagConstraints baseConstraints) {
		baseConstraints.gridwidth = 1;

		grid.add(txtValue, baseConstraints);

		baseConstraints.gridx = col + 1;

		final Dimension btnSize = btnOpenDialog.getPreferredSize();
		final Dimension txtSize = txtValue.getPreferredSize();

		if (btnSize != null && txtSize != null)
			btnOpenDialog.setPreferredSize(new Dimension(btnSize.width, txtSize.height));

		grid.add(btnOpenDialog, baseConstraints);

		btnOpenDialog.addActionListener(e -> openLOVDialog());
	}

	/**
	 * Open the list-of-values dialog
	 */
	@SuppressWarnings("unchecked")
	protected void openLOVDialog() {
		final String dialogClass = getSearchField().getListOfValues();

		try {
			final String filter = txtValue.getText();
			final var cl = (Class<AbstractLOVDialog<?>>) Class.forName(dialogClass);
			final AbstractLOVDialog<?> dlg = cl.getConstructor(Component.class, String.class, Boolean.TYPE)
					.newInstance(getSearchInputDialog(), filter, Boolean.TRUE);

			dlg.setModal(true);
			dlg.setVisible(true);

			if (dlg.getReturnCode() == JTitleAreaDialog.RETURN_CODE_OK) {
				final String text = dlg.getDisplayValue();
				txtValue.setText(text);

				final List<String> vals = Operators.SPLIT_IN.split(text);

				if (vals.size() > 1)
					getCriterion().setSelectedOperator(SearchService.OPERATOR_IN);
			}
		}
		catch (final Exception e) {
			logger.error("Error while opening list of values dialog '{}'!", dialogClass, e);

			getSearchInputDialog().setErrorMessage(getTranslation(LOV_FIELD_MSG_OPERATION_FAILED) + e.getMessage());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#clear()
	 */
	@Override
	public void clear() {
		txtValue.setText("");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#validateAndSet(boolean)
	 */
	@Override
	public void validateAndSet(boolean first) {
		final String text = txtValue.getText();

		if (text.isEmpty()) {
			getSearchInputDialog().setErrorMessage(getTranslation(LOV_FIELD_MSG_MISSING_INPUT));
			txtValue.requestFocus();
		}
		else {
			final List<String> vals = Operators.SPLIT_IN.split(text);
			final SearchOperatorDTO op = getCriterion().getSelectedOperator();

			if (vals.size() > 1 && !Operators.allowsMultipleValues(op)) {
				getSearchInputDialog().setErrorMessage(getTranslation(LOV_FIELD_MSG_IN_OPERATOR));
				txtValue.requestFocus();
			}
			else
				getSearchField().setFilterCriteria(text);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#removeFrom(java.awt.Container)
	 */
	@Override
	public void removeFrom(Container grid) {
		grid.remove(btnOpenDialog);
		grid.remove(txtValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#syncWithSearchField(boolean)
	 */
	@Override
	public void syncWithSearchField(boolean isFirst) {
		txtValue.setText(getSearchField().getFilterCriteria());
	}

}
