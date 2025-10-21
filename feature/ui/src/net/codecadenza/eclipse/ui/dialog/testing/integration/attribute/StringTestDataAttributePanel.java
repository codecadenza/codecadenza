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
package net.codecadenza.eclipse.ui.dialog.testing.integration.attribute;

import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.testing.AssertionOperator;
import net.codecadenza.eclipse.model.testing.TestDataAttribute;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for test data attributes that are mapped to {@link String} values
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class StringTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private static final String EMPTY_VALUE = "EMPTY";

	private Text txtValue;
	private Menu mnuValue;
	private boolean singleLineMode = true;
	private boolean isValueEmpty;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public StringTestDataAttributePanel(TestDataAttributePanelData initializationData) {
		super(initializationData);

		initPanel();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#initPanel()
	 */
	@Override
	protected void initPanel() {
		super.initPanel();

		initTextField();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#
	 * validateAndApplyInput()
	 */
	@Override
	public void validateAndApplyInput() {
		testDataAttribute.setOperator(getSelectedOperator());

		// If necessary, reset the referenced attribute
		if (testDataAttribute.getReferencedAttribute() != null
				&& !txtValue.getText().equals(TRACKING_PREFIX + testDataAttribute.getReferencedAttribute().getId()))
			testDataAttribute.setReferencedAttribute(null);

		if (isValueEmpty || getSelectedOperator() == AssertionOperator.IS_EMPTY)
			testDataAttribute.setValue("");
		else if (txtValue.getText().isEmpty() || testDataAttribute.getReferencedAttribute() != null)
			testDataAttribute.setValue(null);
		else
			testDataAttribute.setValue(txtValue.getText());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#
	 * onSelectedOperatorChanged()
	 */
	@Override
	protected void onSelectedOperatorChanged() {
		if (!isEditable())
			singleLineMode = true;

		initTextField();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.attribute.AbstractTestDataAttributePanel#
	 * onSetReference(net.codecadenza.eclipse.model.domain.DomainObject, net.codecadenza.eclipse.model.testing.TestDataAttribute)
	 */
	@Override
	protected void onSetReference(DomainObject domainObject, TestDataAttribute trackedAttribute) {
		if (isEditable() && testDataAttribute.isReferenceAllowed(domainObject)) {
			testDataAttribute.setReferencedAttribute(trackedAttribute);
			testDataAttribute.setValue(null);

			txtValue.setText(TRACKING_PREFIX + trackedAttribute.getId());

			setBackgroundColor(txtValue);
		}
	}

	/**
	 * Initialize the text field
	 */
	private void initTextField() {
		String lastValue = null;

		if (txtValue != null) {
			lastValue = txtValue.getText();
			txtValue.dispose();
		}

		if (mnuValue != null)
			mnuValue.dispose();

		if (singleLineMode)
			txtValue = new Text(this, SWT.BORDER);
		else
			txtValue = new Text(this, SWT.MULTI | SWT.WRAP | SWT.BORDER | SWT.V_SCROLL);

		final GridData gdTxtValue = new GridData(SWT.FILL, SWT.CENTER, true, false);

		if (!singleLineMode)
			gdTxtValue.heightHint = 150;

		txtValue.setLayoutData(gdTxtValue);
		txtValue.setEditable(isEditable());

		setBackgroundColor(txtValue);

		if (isEditable()) {
			if (lastValue != null)
				txtValue.setText(lastValue);
			else if (testDataAttribute.getReferencedAttribute() != null)
				txtValue.setText(TRACKING_PREFIX + testDataAttribute.getReferencedAttribute().getId());
			else if (testDataAttribute.getValue() != null) {
				if (!validationMode && testDataAttribute.getValue().isEmpty())
					isValueEmpty = true;

				txtValue.setText(testDataAttribute.getValue());
			}

			if (!isValueEmpty) {
				initDropListener(txtValue);

				if (singleLineMode)
					txtValue.setToolTipText("Press F5 to switch to multi-line mode");
				else
					txtValue.setToolTipText("Press F5 to switch to single-line mode");

				addKeyListener();
			}
			else {
				txtValue.setText(EMPTY_VALUE);
				txtValue.setEditable(false);
				txtValue.setBackground(READONLY_COLOR);
			}
		}

		if (!validationMode)
			addMenu();

		getParent().layout(true, true);
	}

	/**
	 * Add a menu to the text field
	 */
	private void addMenu() {
		mnuValue = new Menu(txtValue);
		txtValue.setMenu(mnuValue);

		final var mniSetEmpty = new MenuItem(mnuValue, SWT.NONE);
		mniSetEmpty.setText("Set empty");

		mniSetEmpty.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				setEmpty(true);
			}
		});

		final var mniClear = new MenuItem(mnuValue, SWT.NONE);
		mniClear.setText("Clear");

		mniClear.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				setEmpty(false);
			}
		});
	}

	/**
	 * Add a key listener to the text field
	 */
	private void addKeyListener() {
		txtValue.addKeyListener(new KeyListener() {
			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyReleased(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyReleased(org.eclipse.swt.events.KeyEvent e) {
				if (e.keyCode != SWT.F5)
					return;

				singleLineMode = !singleLineMode;

				initTextField();
			}

			/*
			 * (non-Javadoc)
			 * @see java.awt.event.KeyAdapter#keyPressed(java.awt.event.KeyEvent)
			 */
			@Override
			public void keyPressed(org.eclipse.swt.events.KeyEvent e) {
				// No implementation required!
			}
		});
	}

	/**
	 * @param empty flag that controls if the {@link TestDataAttribute} should be initialized with an empty value
	 */
	private void setEmpty(boolean empty) {
		testDataAttribute.setReferencedAttribute(null);
		isValueEmpty = empty;

		if (empty)
			txtValue.setText("");

		initTextField();
	}

}
