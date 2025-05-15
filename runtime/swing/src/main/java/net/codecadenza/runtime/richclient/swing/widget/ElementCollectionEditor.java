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
package net.codecadenza.runtime.richclient.swing.widget;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ELEMENT_COLLECTION_EDITOR_ACTION_DELETE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ELEMENT_COLLECTION_EDITOR_ACTION_DELETE_ALL;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ELEMENT_COLLECTION_EDITOR_CMD_ADD;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ELEMENT_COLLECTION_EDITOR_LBL_ADD;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslationForFieldLabel;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import net.codecadenza.runtime.conversion.ValueConverter;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;

/**
 * <p>
 * Editor for maintaining element collections
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of an element in the collection
 */
public class ElementCollectionEditor<T> extends JPanel {
	private static final long serialVersionUID = -3614323560956773012L;

	protected final FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	protected final SimpleDateFormat dateTimeFormat = new SimpleDateFormat(userFormat.getDateTimeFormat(), Locale.getDefault());
	protected final SimpleDateFormat dateFormat = new SimpleDateFormat(userFormat.getDateFormat(), Locale.getDefault());
	protected final DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
	protected final boolean readonly;
	protected final Class<T> elementType;
	protected final DefaultListModel<String> listModel = new DefaultListModel<>();
	protected final transient ValueConverter<T> valueConverter;
	protected transient Collection<T> elements = java.util.Collections.emptyList();
	protected JList<String> elementList;

	/**
	 * Constructor
	 * @param readonly
	 * @param elementType
	 */
	public ElementCollectionEditor(boolean readonly, Class<T> elementType) {
		this.readonly = readonly;
		this.elementType = elementType;
		this.valueConverter = new ValueConverter<>(userFormat.getDecimalFormat(), userFormat.getDateTimeFormat(),
				userFormat.getDateFormat(), elementType);

		initControl();
	}

	/**
	 * Set the initial elements
	 * @param elements the initial elements
	 */
	public void setElements(Collection<T> elements) {
		this.elements = elements;

		refreshList(null);
	}

	/**
	 * @return the list component
	 */
	public JList<String> getElementList() {
		return elementList;
	}

	/**
	 * Initialize the control
	 */
	protected void initControl() {
		setLayout(new BorderLayout());

		if (!readonly) {
			final var lblAdd = new JLabel(getTranslationForFieldLabel(ELEMENT_COLLECTION_EDITOR_LBL_ADD));

			final var txtAdd = new JTextField(30);
			txtAdd.setText(valueConverter.getInitialDefaultValue());

			txtAdd.addKeyListener(new KeyAdapter() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.KeyAdapter#keyReleased(java.awt.event.KeyEvent)
				 */
				@Override
				public void keyReleased(KeyEvent e) {
					refreshList(txtAdd.getText());
				}
			});

			final var cmdAdd = new JButton(getTranslation(ELEMENT_COLLECTION_EDITOR_CMD_ADD));

			cmdAdd.addActionListener(e -> {
				if (txtAdd.getText().isEmpty())
					return;

				try {
					final T newElement = valueConverter.convertToValue(txtAdd.getText());

					elements.add(newElement);
					refreshList(null);
				}
				catch (final Exception ex) {
					JOptionPane.showMessageDialog(this, getTranslation(ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED, ex.getMessage()),
							getTranslation(ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION), JOptionPane.INFORMATION_MESSAGE);
				}
			});

			final var panAdd = new JPanel();
			panAdd.setLayout(new FlowLayout(FlowLayout.LEFT));
			panAdd.add(lblAdd);
			panAdd.add(txtAdd);
			panAdd.add(cmdAdd);

			add(panAdd, BorderLayout.NORTH);
		}

		elementList = new JList<>(listModel);
		elementList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		if (!readonly) {
			elementList.addKeyListener(new KeyAdapter() {
				/*
				 * (non-Javadoc)
				 * @see java.awt.event.KeyAdapter#keyReleased(java.awt.event.KeyEvent)
				 */
				@Override
				public void keyPressed(KeyEvent e) {
					if (e.getKeyCode() == KeyEvent.VK_DELETE)
						deleteSelectedItem();
				}
			});

			final var mniDelete = new JMenuItem(getTranslation(ELEMENT_COLLECTION_EDITOR_ACTION_DELETE));
			mniDelete.addActionListener(e -> deleteSelectedItem());

			final var mniDeleteAll = new JMenuItem(getTranslation(ELEMENT_COLLECTION_EDITOR_ACTION_DELETE_ALL));

			mniDeleteAll.addActionListener(e -> {
				elements.clear();
				refreshList(null);
			});

			final var contextMenu = new JPopupMenu();
			contextMenu.add(mniDelete);
			contextMenu.add(mniDeleteAll);

			elementList.setComponentPopupMenu(contextMenu);
		}

		add(new JScrollPane(elementList), BorderLayout.CENTER);
	}

	/**
	 * Refresh the {@link List}
	 * @param filter the filter for elements to be displayed
	 */
	protected void refreshList(String filter) {
		final List<String> filteredElements;

		if (filter != null && !filter.isEmpty())
			filteredElements = elements.stream().sorted().map(valueConverter::convertToString).filter(item -> item.startsWith(filter))
					.toList();
		else
			filteredElements = elements.stream().sorted().map(valueConverter::convertToString).toList();

		listModel.clear();
		listModel.addAll(filteredElements);
	}

	/**
	 * Delete the selected item
	 */
	protected void deleteSelectedItem() {
		final var selectedValue = elementList.getSelectedValue();

		if (selectedValue != null) {
			elements.remove(valueConverter.convertToValue(selectedValue));
			refreshList(null);
		}
	}
}
