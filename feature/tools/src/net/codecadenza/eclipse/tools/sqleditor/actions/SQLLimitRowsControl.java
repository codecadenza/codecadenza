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
package net.codecadenza.eclipse.tools.sqleditor.actions;

import static net.codecadenza.eclipse.shared.Constants.PREF_MAX_ROW_COUNT;

import org.eclipse.jface.action.ControlContribution;
import org.eclipse.jface.action.StatusLineManager;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Control to limit the number of returned rows
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLLimitRowsControl extends ControlContribution {
	private Button chkLimitResults;
	private Text txtMaxResult;

	/**
	 * Constructor
	 */
	public SQLLimitRowsControl() {
		super(SQLLimitRowsControl.class.getName());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createControl(Composite parent) {
		// Get the preference store
		final IPreferenceStore store = net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin.getInstance().getPreferenceStore();
		final int maxRowCount = store.getInt(PREF_MAX_ROW_COUNT);

		final var glControlArea = new GridLayout(3, false);
		glControlArea.marginHeight = 0;

		final var panControlArea = new Composite(parent, SWT.NONE);
		panControlArea.setLayout(glControlArea);
		panControlArea.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var statusMgr = new StatusLineManager();
		statusMgr.createControl(panControlArea);

		chkLimitResults = new Button(panControlArea, SWT.CHECK);
		chkLimitResults.setText("Limit rows");
		chkLimitResults.setSelection(true);

		final var gdMaxResult = new GridData(SWT.LEFT, SWT.CENTER, false, false);
		gdMaxResult.widthHint = 50;

		txtMaxResult = new Text(panControlArea, SWT.BORDER | SWT.SINGLE);
		txtMaxResult.setText(Integer.toString(maxRowCount));
		txtMaxResult.setLayoutData(gdMaxResult);

		chkLimitResults.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseUp(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseUp(MouseEvent e) {
				txtMaxResult.setEnabled(chkLimitResults.getSelection());
			}
		});

		panControlArea.layout();

		return panControlArea;
	}

	/**
	 * Get the maximum number of records to be returned by a query
	 * @return the maximum number of rows to fetch. The method will return 0 if all available rows should be fetched!
	 */
	public int getLimitResults() {
		if (!chkLimitResults.getSelection())
			return 0;

		try {
			return Integer.parseInt(txtMaxResult.getText());
		}
		catch (final NumberFormatException _) {
			// This exception will be ignored!
		}

		return 0;
	}

}
