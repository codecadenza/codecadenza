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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.ABSTRACT_PROPOSAL_TEXT_FIELD_DEFAULT_ITEM_LABEL;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.IllegalComponentStateException;
import java.awt.Point;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.Popup;
import javax.swing.PopupFactory;
import javax.swing.border.LineBorder;

/**
 * <p>
 * Abstract proposal text field
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the proposal text field
 */
public abstract class AbstractProposalTextField<T> extends JTextField {
	private static final int POPUP_HEIGHT = 200;
	private static final int MAX_NUMBER_OF_ELEMENTS = 10;
	private static final int DEFAULT_FILTER_LENGTH = 2;
	private static final long serialVersionUID = -849784387224186668L;

	private transient Popup popup;
	private JDataTable<T> dataTable;
	private transient T selectedElement;
	private final transient PopupFactory factory = PopupFactory.getSharedInstance();
	private boolean popUpIsVisible;
	private JScrollPane scrollPane;
	private int filterLength = DEFAULT_FILTER_LENGTH;
	private JRootPane rootPane;
	private JButton defaultButton;

	/**
	 * @param filter
	 * @return the proposal list
	 */
	public abstract Collection<T> getProposalData(String filter);

	/**
	 * @param selectedElement
	 * @return the text to be displayed
	 */
	public abstract String getDisplayText(T selectedElement);

	/**
	 * Constructor
	 * @param parent
	 * @param filterLength number of characters that must be inserted into field before proposal data is fetched
	 */
	protected AbstractProposalTextField(final Component parent, int filterLength) {
		this(parent);

		if (filterLength < 1)
			this.filterLength = 1;
		else
			this.filterLength = filterLength;
	}

	/**
	 * Constructor
	 * @param parent
	 */
	protected AbstractProposalTextField(final Component parent) {
		// Build the table renderer
		final var renderer = new AbstractDataTableRenderer<T>() {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellText(java.lang.Object, int)
			 */
			@Override
			protected String getCellText(T value, int column) {
				return AbstractProposalTextField.this.getDisplayText(value);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellImage(java.lang.Object, int)
			 */
			@Override
			protected Icon getCellImage(T value, int column) {
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellFont(java.lang.Object, int)
			 */
			@Override
			protected Font getCellFont(T value, int column) {
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellForeground(java.lang.Object, int)
			 */
			@Override
			protected Color getCellForeground(T value, int column) {
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellBackground(java.lang.Object, int)
			 */
			@Override
			protected Color getCellBackground(T value, int column) {
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellValue(java.lang.Object, int)
			 */
			@Override
			public Comparable<?> getCellValue(T value, int column) {
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer#getCellExportText(java.lang.Object, int)
			 */
			@Override
			protected String getCellExportText(T element, int columnIndex) {
				return "";
			}
		};

		// We just have to define one column
		final var list = new ArrayList<ColumnInfo>();
		list.add(new ColumnInfo(1, getTranslation(ABSTRACT_PROPOSAL_TEXT_FIELD_DEFAULT_ITEM_LABEL), 100));

		// Initialize the table that will be used to display proposal data
		dataTable = new JDataTable<>(renderer, null);
		dataTable.setShowGrid(false);
		dataTable.setSortable(false);
		dataTable.initColumns(list);

		dataTable.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.MouseAdapter#mouseClicked(java.awt.event.MouseEvent)
			 */
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() != 2)
					return;

				makeSelection();
			}
		});

		dataTable.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyPressed(KeyEvent event) {
				// We reset the selected element automatically!
				selectedElement = null;

				if (event.getKeyCode() == KeyEvent.VK_ENTER) {
					makeSelection();

					// Request focus for the proposal text field. Otherwise it is very likely that a completely different field gains the
					// focus!
					AbstractProposalTextField.this.requestFocusInWindow();
					return;
				}

				if (event.getKeyCode() == KeyEvent.VK_TAB)
					AbstractProposalTextField.this.requestFocusInWindow();
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyReleased(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				// No implementation required!
			}
		});

		dataTable.addFocusListener(new FocusListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				hidePopUp();
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.FocusListener#focusGained(java.awt.event.FocusEvent)
			 */
			@Override
			public void focusGained(FocusEvent e) {
				// No implementation required!
			}
		});

		addFocusListener(new FocusListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.FocusListener#focusLost(java.awt.event.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent arg0) {
				// If the component looses the focus the original default button again can be restored!
				if (rootPane != null)
					rootPane.setDefaultButton(defaultButton);
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.FocusListener#focusGained(java.awt.event.FocusEvent)
			 */
			@Override
			public void focusGained(FocusEvent arg0) {
				// If the component gets the focus we must reset the root pane's default button!
				if (parent == null)
					return;

				if (AbstractProposalTextField.this.getParent() instanceof final JComponent parentComponent) {
					rootPane = parentComponent.getRootPane();
					defaultButton = rootPane.getDefaultButton();
					rootPane.setDefaultButton(null);
				}
			}
		});

		scrollPane = new JScrollPane();
		scrollPane.setViewportView(dataTable);
		scrollPane.setBorder(new LineBorder(getDisabledTextColor()));

		this.add(scrollPane, BorderLayout.CENTER);

		addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ENTER) {
					// If the user selects an item in the pop-up data table by pressing 'ENTER' the event will be fired also for the
					// proposal text field. Thus, we must exit this method because otherwise the pop-up data table would be opened again!
					makeSelection();
					return;
				}

				// It should be possible to navigate through the proposal list by using arrow up and arrow down keys!
				if (e.getKeyCode() == KeyEvent.VK_DOWN && dataTable.getRowCount() > 0) {
					final ListSelectionModel selectionModel = dataTable.getSelectionModel();
					int selIndex = dataTable.getSelectionModel().getMinSelectionIndex();

					if (selIndex == -1)
						selIndex = 0;
					else
						selIndex++;

					if (selIndex < dataTable.getRowCount())
						selectionModel.setSelectionInterval(0, selIndex);

					return;
				}

				if (e.getKeyCode() == KeyEvent.VK_UP && dataTable.getRowCount() > 0) {
					final ListSelectionModel selectionModel = dataTable.getSelectionModel();
					int selIndex = dataTable.getSelectionModel().getMinSelectionIndex();

					if (selIndex > 1)
						selIndex--;
					else
						selIndex = 0;

					selectionModel.setSelectionInterval(0, selIndex);

					return;
				}

				final String filter = getText();

				if (!filter.isEmpty() && filter.length() >= filterLength) {
					final ArrayList<T> listDisplay = new ArrayList<>(AbstractProposalTextField.this.getProposalData(filter));

					if (!listDisplay.isEmpty()) {
						if (listDisplay.size() > MAX_NUMBER_OF_ELEMENTS)
							dataTable.setData(listDisplay.subList(0, MAX_NUMBER_OF_ELEMENTS - 1));
						else
							dataTable.setData(listDisplay);

						showPopUp();
						return;
					}
				}
				else
					dataTable.setData(new ArrayList<>());

				hidePopUp();
			}
		});

		// We keep track of visual changes of the parent component
		parent.addComponentListener(new ComponentListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ComponentListener#componentShown(java.awt.event.ComponentEvent)
			 */
			@Override
			public void componentShown(ComponentEvent e) {
				reInitializePopup();
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ComponentListener#componentResized(java.awt.event.ComponentEvent)
			 */
			@Override
			public void componentResized(ComponentEvent e) {
				reInitializePopup();
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ComponentListener#componentMoved(java.awt.event.ComponentEvent)
			 */
			@Override
			public void componentMoved(ComponentEvent e) {
				reInitializePopup();
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.ComponentListener#componentHidden(java.awt.event.ComponentEvent)
			 */
			@Override
			public void componentHidden(ComponentEvent e) {
				// No implementation required!
			}
		});
	}

	/**
	 * Get the selected object from the internal data table and hand it over to the parent component
	 */
	private void makeSelection() {
		setSelectedElement(dataTable.getSelectedElement());

		// As soon as the user selects an element we hide the pop-up control!
		hidePopUp();
	}

	/**
	 * Reinitialize the pop-up control and hide the existing pop-up first!
	 */
	private void reInitializePopup() {
		// If the pop-up component already exists we will hide it first
		hidePopUp();

		try {
			initializePopup();
		}
		catch (final IllegalComponentStateException _) {
			// The pop-up cannot be initialized at the moment as it isn't in an appropriate state (e.g. not visible on screen)!
		}
	}

	/**
	 * @param selectedElement
	 */
	@SuppressWarnings("unused")
	public void onSelectionChanged(T selectedElement) {

	}

	/**
	 * Hide the pop-up control
	 */
	private void hidePopUp() {
		if (popup != null && popUpIsVisible) {
			popUpIsVisible = false;
			popup.hide();
		}
	}

	/**
	 * Show the pop-up control
	 */
	private void showPopUp() {
		// We just have to show the pop-up control if it is not visible yet!
		if (!popUpIsVisible) {
			try {
				initializePopup();
			}
			catch (final IllegalComponentStateException _) {
				// If we get an exception here we don't show the pop-up!
				return;
			}

			popup.show();
			popUpIsVisible = true;
		}
	}

	/**
	 * Initialize the pop-up
	 */
	private void initializePopup() {
		final Point p = getLocationOnScreen();

		scrollPane.setPreferredSize(new Dimension(getWidth(), POPUP_HEIGHT));
		popup = factory.getPopup(getParent(), scrollPane, p.x, p.y + getHeight() - 1);
	}

	/**
	 * @return the selected element
	 */
	public T getSelectedElement() {
		return selectedElement;
	}

	/**
	 * @param selectedElement
	 */
	public void setSelectedElement(T selectedElement) {
		this.selectedElement = selectedElement;

		if (selectedElement != null)
			setText(getDisplayText(selectedElement));
		else
			setText("");

		onSelectionChanged(selectedElement);
	}

}
