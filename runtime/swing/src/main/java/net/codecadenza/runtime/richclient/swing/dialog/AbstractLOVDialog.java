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
package net.codecadenza.runtime.richclient.swing.dialog;

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOV_DIALOG_CMD_DESELECT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOV_DIALOG_CMD_FILTER;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOV_DIALOG_INFO_MESSAGE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOV_DIALOG_LBL_FILTER;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_LOV_DIALOG_MSG_NO_ITEM_SELECTED;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslationForFieldLabel;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.text.DecimalFormat;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.EmptyBorder;
import net.codecadenza.runtime.richclient.swing.image.ImageLoader;
import net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel;
import net.codecadenza.runtime.richclient.swing.widget.ColumnInfo;
import org.slf4j.Logger;

/**
 * <p>
 * Abstract base class for list-of-value dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the LOV dialog
 */
public abstract class AbstractLOVDialog<T> extends JTitleAreaDialog {
	private static final long serialVersionUID = 6372224495901543547L;

	private JTextField txtFilter;
	private String idValue;
	private String displayValue;
	protected boolean allowMultipleSelection;
	private boolean allowDeselection;
	protected AbstractDataTablePanel<T> dataPanel;
	private String filter = "";
	protected transient DateTimeFormatter dateFormat;
	protected transient DateTimeFormatter dateTimeFormat;
	protected DecimalFormat decimalFormat;

	/**
	 * Constructor
	 * @param parent
	 * @param filter
	 * @param allowMultipleSelection
	 * @param allowDeselection
	 */
	protected AbstractLOVDialog(Component parent, String filter, boolean allowMultipleSelection, boolean allowDeselection) {
		super(parent);

		this.filter = filter;
		this.allowMultipleSelection = allowMultipleSelection;
		this.allowDeselection = allowDeselection;
	}

	/**
	 * Constructor
	 * @param parent
	 * @param filter
	 * @param allowMultipleSelection
	 */
	protected AbstractLOVDialog(Component parent, String filter, boolean allowMultipleSelection) {
		this(parent, filter, allowMultipleSelection, false);
	}

	/**
	 * Constructor
	 * @param parent
	 * @param filter
	 */
	protected AbstractLOVDialog(Component parent, String filter) {
		this(parent, filter, false, false);
	}

	/**
	 * Set the ID value
	 * @param selectedElement
	 * @return the ID value
	 */
	public abstract String setIdValue(T selectedElement);

	/**
	 * Set the display value
	 * @param selectedElements
	 * @return the display value
	 */
	public abstract String setDisplayValue(List<T> selectedElements);

	/**
	 * @return the column definition list
	 */
	public abstract List<ColumnInfo> getColumnDefinition();

	/**
	 * Fetch data
	 * @param filter
	 * @return a list of data to be displayed
	 */
	public abstract List<T> fetchData(String filter);

	/**
	 * @param element
	 * @param columnIndex
	 * @return the text of this cell
	 */
	public abstract String getCellText(T element, int columnIndex);

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getLogger();

	/**
	 * @return true if multiple selection is enabled
	 */
	public boolean isAllowMultipleSelection() {
		return allowMultipleSelection;
	}

	/**
	 * Set the return values
	 */
	private void setReturnValues() {
		final T singleSel = dataPanel.getSelectedElement();
		final List<T> multiSel = dataPanel.getAllSelectedElements();

		idValue = setIdValue(singleSel);
		displayValue = setDisplayValue(multiSel);
	}

	/**
	 * Get the cell background. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell background color
	 */
	@SuppressWarnings("unused")
	public Color getCellBackground(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell foreground color. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell foreground color
	 */
	@SuppressWarnings("unused")
	public Color getCellForeground(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell font. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the font
	 */
	@SuppressWarnings("unused")
	public Font getCellFont(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell image. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell image
	 */
	@SuppressWarnings("unused")
	public Icon getCellImage(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell's value for sorting. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the comparable value
	 */
	@SuppressWarnings("unused")
	public Comparable<?> getCellValue(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell export text. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the string interpretation for export
	 */
	public String getCellExportText(T element, int columnIndex) {
		return getCellText(element, columnIndex);
	}

	/**
	 * @return the ID value
	 */
	public String getIdValue() {
		return idValue;
	}

	/**
	 * @return the display value
	 */
	public String getDisplayValue() {
		return displayValue;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#createContents(javax.swing.JPanel)
	 */
	@Override
	public void createContents(JPanel contentPane) {
		getLogger().debug("Initialize dialog");

		setSize(450, 400);
		setLocationRelativeTo(null);
		setTitleImage(ImageLoader.getImage(ImageLoader.LOV_TITLE));
		setInformationMessage(getTranslation(ABSTRACT_LOV_DIALOG_INFO_MESSAGE));

		contentPane.setLayout(new BorderLayout());

		final var panFilter = new JPanel();
		panFilter.setBorder(new EmptyBorder(5, 5, 5, 5));

		contentPane.add(panFilter, BorderLayout.NORTH);

		final var gblFilterPanel = new GridBagLayout();
		gblFilterPanel.columnWidths = new int[] { 50, 200, 50, 0 };
		gblFilterPanel.rowHeights = new int[] { 23, 0 };
		gblFilterPanel.columnWeights = new double[] { 0.0, 0.0, 0.0, Double.MIN_VALUE };
		gblFilterPanel.rowWeights = new double[] { 0.0, Double.MIN_VALUE };

		panFilter.setLayout(gblFilterPanel);

		final var lblFilter = new JLabel(getTranslationForFieldLabel(ABSTRACT_LOV_DIALOG_LBL_FILTER));

		final var gbcFilterLabel = new GridBagConstraints();
		gbcFilterLabel.anchor = GridBagConstraints.WEST;
		gbcFilterLabel.fill = GridBagConstraints.VERTICAL;
		gbcFilterLabel.insets = new Insets(0, 0, 0, 5);
		gbcFilterLabel.gridx = 0;
		gbcFilterLabel.gridy = 0;

		panFilter.add(lblFilter, gbcFilterLabel);

		txtFilter = new JTextField();
		txtFilter.setText(filter);

		txtFilter.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyReleased(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent event) {
				dataPanel.performFetch();
			}
		});

		txtFilter.addFocusListener(new FocusListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent arg0) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.FocusListener#focusGained(java.awt.event.FocusEvent)
			 */
			@Override
			public void focusGained(FocusEvent arg0) {
				// If the filter text field gains the focus we must reset the dialog's default button. Otherwise pressing the enter key
				// within the filter text field would trigger this button.
				getRootPane().setDefaultButton(null);
			}
		});

		final var gdcFilter = new GridBagConstraints();
		gdcFilter.fill = GridBagConstraints.HORIZONTAL;
		gdcFilter.insets = new Insets(0, 0, 0, 5);
		gdcFilter.gridx = 1;
		gdcFilter.gridy = 0;

		panFilter.add(txtFilter, gdcFilter);

		txtFilter.setColumns(10);

		final var cmdFilter = new JButton(getTranslation(ABSTRACT_LOV_DIALOG_CMD_FILTER));
		cmdFilter.addActionListener(action -> dataPanel.performFetch());

		getRootPane().setDefaultButton(cmdFilter);

		final var gbcFilterButton = new GridBagConstraints();
		gbcFilterButton.fill = GridBagConstraints.BOTH;
		gbcFilterButton.gridx = 2;
		gbcFilterButton.gridy = 0;

		panFilter.add(cmdFilter, gbcFilterButton);

		dataPanel = new AbstractDataTablePanel<>(allowMultipleSelection, true) {
			private static final long serialVersionUID = -7951749373644799028L;

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#fetchData()
			 */
			@Override
			public List<T> fetchData() {
				if (txtFilter.getText().isEmpty())
					return Collections.emptyList();

				return AbstractLOVDialog.this.fetchData(txtFilter.getText());
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#onDblClick(java.lang.Object)
			 */
			@Override
			public void onDblClick(Object selectedElement) {
				onOKClicked();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellBackground(java.lang.Object, int)
			 */
			@Override
			public Color getCellBackground(T element, int columnIndex) {
				return AbstractLOVDialog.this.getCellBackground(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellFont(java.lang.Object, int)
			 */
			@Override
			public Font getCellFont(T element, int columnIndex) {
				return AbstractLOVDialog.this.getCellFont(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellForeground(java.lang.Object, int)
			 */
			@Override
			public Color getCellForeground(T element, int columnIndex) {
				return AbstractLOVDialog.this.getCellForeground(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellImage(java.lang.Object, int)
			 */
			@Override
			public Icon getCellImage(T element, int columnIndex) {
				return AbstractLOVDialog.this.getCellImage(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(T element, int columnIndex) {
				return AbstractLOVDialog.this.getCellText(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellValue(java.lang.Object, int)
			 */
			@Override
			public java.lang.Comparable<?> getCellValue(T element, int columnIndex) {
				return AbstractLOVDialog.this.getCellValue(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getColumnDefinition()
			 */
			@Override
			public List<ColumnInfo> getColumnDefinition() {
				return AbstractLOVDialog.this.getColumnDefinition();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getCellExportText(java.lang.Object, int)
			 */
			@Override
			public String getCellExportText(T element, int columnIndex) {
				return AbstractLOVDialog.this.getCellExportText(element, columnIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.DeleteKeyPressedListener#onDeleteKeyPressed(java.
			 * lang.Object)
			 */
			@Override
			public void onDeleteKeyPressed(Object selectedElement) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.JDataTable.EnterKeyPressedListener#onEnterKeyPressed(java.
			 * lang.Object)
			 */
			@Override
			public void onEnterKeyPressed(Object selectedElement) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getLogger()
			 */
			@Override
			protected Logger getLogger() {
				return AbstractLOVDialog.this.getLogger();
			}
		};

		dateFormat = dataPanel.getDateFormat();
		dateTimeFormat = dataPanel.getDateTimeFormat();
		decimalFormat = dataPanel.getDecimalFormat();

		// The filter text field should gain the focus!
		txtFilter.requestFocusInWindow();

		contentPane.add(dataPanel, BorderLayout.CENTER);

		getLogger().debug("Dialog initialization finished");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#createButtons(javax.swing.JPanel)
	 */
	@Override
	protected void createButtons(JPanel buttonPane) {
		super.createButtons(buttonPane);

		if (allowDeselection) {
			final var deselectButton = new JButton(getTranslation(ABSTRACT_LOV_DIALOG_CMD_DESELECT));

			deselectButton.addActionListener(action -> {
				idValue = null;
				displayValue = null;
				setReturnCode(RETURN_CODE_OK);
				dispose();
			});

			buttonPane.add(deselectButton);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#onOKClicked()
	 */
	@Override
	public void onOKClicked() {
		setReturnValues();

		if (idValue == null) {
			setInformationMessage(getTranslation(ABSTRACT_LOV_DIALOG_MSG_NO_ITEM_SELECTED));
			return;
		}

		setReturnCode(RETURN_CODE_OK);
		dispose();
	}

}
