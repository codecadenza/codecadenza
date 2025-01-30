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
package net.codecadenza.runtime.richclient.swing.search.input.components;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.CRITERION_MSG_OP_NOT_SUPPORTED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Component;
import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.io.Serializable;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.MutableComboBoxModel;
import net.codecadenza.runtime.richclient.swing.search.input.SearchInputDialog;
import net.codecadenza.runtime.richclient.swing.search.input.field.InputField;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;
import net.codecadenza.runtime.search.util.SearchOperatorHelper;

/**
 * <p>
 * Represents a search criterion. Every criterion is displayed as a row in the {@link FilterSettingsTab}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Criterion implements Serializable {
	private static final long serialVersionUID = 4452171519155412383L;

	private final SearchFieldDTO field;
	private SearchInputDialog dlg;

	// The row number which is needed to correctly place the input fields in the grid
	private final int rowNum;

	private final Container grid;
	protected final JLabel lblName;
	protected final JComboBox<SearchOperatorDTO> cboOp;

	// The input field for the first parameter of the operator
	private InputField input;

	// The input field for the second parameter of the operator (currently only supported for the 'between' operator)
	private InputField inputUpperBound;

	// True if this row is the last row in the grid. The field is necessary to correctly set the layout constraints
	private boolean last;

	/**
	 * Constructor
	 * @param field
	 * @param grid
	 * @param rowNum
	 */
	public Criterion(SearchFieldDTO field, Container grid, int rowNum) {
		this.field = field;
		this.grid = grid;
		this.rowNum = rowNum;
		this.lblName = new JLabel(field.getColLabel());
		this.cboOp = new JComboBox<>();
	}

	/**
	 * Add this row to the grid
	 * @param last true if this is the last row in the grid
	 */
	public void addToGrid(boolean last) {
		if (dlg == null)
			throw new IllegalStateException();

		this.last = last;

		final GridBagConstraints cs = createBaseConstraints(rowNum, 0);
		grid.add(lblName, cs);

		cs.gridx = 1;

		if (last)
			cs.anchor = GridBagConstraints.NORTH;

		grid.add(cboOp, cs);

		final var model = (MutableComboBoxModel<SearchOperatorDTO>) cboOp.getModel();
		model.addElement(null);

		SearchOperatorHelper.getOperatorsForField(field).forEach(model::addElement);

		cboOp.setRenderer(new OpRenderer());
		cboOp.setEditable(false);
		cboOp.addItemListener(e -> updateInputFields());

		syncWithSearchField();
	}

	/**
	 * Synchronize with search field
	 */
	private void syncWithSearchField() {
		final SearchOperatorDTO op = field.getOperator();

		if (op == null)
			setSelectedOperator(null);
		else
			setSelectedOperator(op.getValue());

		updateInputFields();

		if (op != null) {
			if (input != null)
				input.syncWithSearchField(true);

			if (inputUpperBound != null)
				inputUpperBound.syncWithSearchField(false);
		}
	}

	/**
	 * @param r
	 * @param c
	 * @return the layout constraints
	 */
	private GridBagConstraints createBaseConstraints(int r, int c) {
		return createBaseConstraints(r, c, 1);
	}

	/**
	 * @param r
	 * @param c
	 * @param w
	 * @return the generated layout constraints
	 */
	private GridBagConstraints createBaseConstraints(int r, int c, int w) {
		final var cs = new GridBagConstraints();
		cs.gridwidth = w;
		cs.gridx = c;
		cs.gridy = r;
		cs.insets = new Insets(3, 3, 0, 0);
		cs.fill = GridBagConstraints.HORIZONTAL;

		if (last)
			cs.anchor = GridBagConstraints.NORTH;

		return cs;
	}

	/**
	 * Renders instances of {@link SearchOperatorDTO}
	 */
	public static class OpRenderer extends DefaultListCellRenderer {
		private static final long serialVersionUID = 8389817802290976594L;

		/*
		 * (non-Javadoc)
		 * @see javax.swing.DefaultListCellRenderer#getListCellRendererComponent(javax.swing.JList, java.lang.Object, int, boolean,
		 * boolean)
		 */
		@Override
		public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected,
				boolean cellHasFocus) {
			final var op = (SearchOperatorDTO) value;
			return super.getListCellRendererComponent(list, value == null ? " " : op.getValue(), index, isSelected, cellHasFocus);
		}
	}

	/**
	 * Reset the parameters
	 */
	public void clear() {
		field.setFilterCriteria(null);
		field.setOperator(null);

		cboOp.setSelectedItem(null);
		updateInputFields();

		if (input != null)
			input.clear();

		if (inputUpperBound != null)
			inputUpperBound.clear();
	}

	/**
	 * Save the last selected operator
	 */
	private int lastOpIndex = -2;

	/**
	 * Update input fields
	 */
	private void updateInputFields() {
		final int index = cboOp.getSelectedIndex();

		if (index == lastOpIndex)
			return;

		lastOpIndex = index;

		final var op = (SearchOperatorDTO) cboOp.getSelectedItem();

		boolean shouldRepaint = false;

		if (input != null && !hasInput(op)) {
			input.removeFrom(grid);
			input = null;

			shouldRepaint = true;
		}
		else if (input == null && hasInput(op)) {
			input = InputField.create(field, this);
			input.setSearchInputDialog(dlg);
			input.addTo(grid, rowNum, 2, createBaseConstraints(rowNum, 2, 2));

			shouldRepaint = true;
		}

		if (inputUpperBound != null && !hasInputBetween(op)) {
			inputUpperBound.removeFrom(grid);
			inputUpperBound = null;

			shouldRepaint = true;
		}
		else if (inputUpperBound == null && hasInputBetween(op)) {
			inputUpperBound = InputField.create(field, this);
			inputUpperBound.setSearchInputDialog(dlg);
			inputUpperBound.addTo(grid, rowNum, 4, createBaseConstraints(rowNum, 4, 2));

			shouldRepaint = true;
		}

		if (shouldRepaint)
			((JComponent) grid).getRootPane().repaint();
	}

	/**
	 * @param op
	 * @return true if the operator expects user input
	 */
	private boolean hasInput(SearchOperatorDTO op) {
		return op != null && !op.getValue().equals(SearchService.OPERATOR_IS_NULL)
				&& !op.getValue().equals(SearchService.OPERATOR_IS_NOT_NULL);
	}

	/**
	 * @param op
	 * @return true if the 'between' operator is selected
	 */
	private boolean hasInputBetween(SearchOperatorDTO op) {
		return op != null && SearchService.OPERATOR_BETWEEN.equals(op.getValue());
	}

	/**
	 * @param field data-model, gets mutated
	 * @param grid the grid to draw on
	 * @param rowNum row number
	 * @return the generated criterion
	 */
	public static Criterion createCriterion(SearchFieldDTO field, Container grid, int rowNum) {
		return new Criterion(field, grid, rowNum);
	}

	/**
	 * Validate the user input and store it in the input field
	 */
	public void validateAndSet() {
		final var op = (SearchOperatorDTO) cboOp.getSelectedItem();

		if (hasInput(op)) {
			input.validateAndSet(true);

			if (!dlg.isErrorState() && SearchService.OPERATOR_BETWEEN.equals(op.getDescription()))
				inputUpperBound.validateAndSet(false);

			if (!dlg.isErrorState())
				input.getSearchField().setOperator(op);
		}
		else {
			field.setOperator(op);
			field.setFilterCriteria(null);
		}
	}

	/**
	 * It is needed to inform the user about errors and has to be set before the criterion is added to the grid
	 * @param dlg the {@link SearchInputDialog} to set
	 */
	public void setSearchInputDialog(SearchInputDialog dlg) {
		this.dlg = dlg;

		if (input != null)
			input.setSearchInputDialog(dlg);

		if (inputUpperBound != null)
			inputUpperBound.setSearchInputDialog(dlg);
	}

	/**
	 * Set the selected operator. If it the parameter is null, reset the operator.
	 * @param op the operator to set
	 */
	public void setSelectedOperator(String op) {
		if (op == null)
			cboOp.setSelectedItem(null);
		else {
			final var mdl = (DefaultComboBoxModel<SearchOperatorDTO>) cboOp.getModel();

			// Search for the selected operator
			for (int i = 0; i < mdl.getSize(); ++i) {
				final SearchOperatorDTO sop = mdl.getElementAt(i);

				if (sop != null && op.equals(sop.getValue())) {
					cboOp.setSelectedIndex(i);
					return;
				}
			}

			throw new IllegalArgumentException(getTranslation(CRITERION_MSG_OP_NOT_SUPPORTED, op));
		}
	}

	/**
	 * @return the selected operator or null if no operator is selected
	 */
	public SearchOperatorDTO getSelectedOperator() {
		return (SearchOperatorDTO) cboOp.getSelectedItem();
	}

}
