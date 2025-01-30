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

import org.eclipse.jface.action.ControlContribution;
import org.eclipse.jface.action.StatusLineManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * <p>
 * Control to set the transaction commit mode
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CommitModeControl extends ControlContribution {
	private Button chkAutoCommit;

	/**
	 * Constructor
	 */
	public CommitModeControl() {
		super(CommitModeControl.class.getName());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.action.ControlContribution#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createControl(Composite parent) {
		final var glControlArea = new GridLayout(2, false);
		glControlArea.marginHeight = 0;

		final var panControlArea = new Composite(parent, SWT.NONE);
		panControlArea.setLayoutData(new GridData(SWT.FILL, SWT.BOTTOM, true, false));
		panControlArea.setLayout(glControlArea);

		final var statusMgr = new StatusLineManager();
		statusMgr.createControl(panControlArea);

		chkAutoCommit = new Button(panControlArea, SWT.CHECK);
		chkAutoCommit.setText("Auto commit");
		chkAutoCommit.setSelection(true);

		panControlArea.layout();

		return panControlArea;
	}

	/**
	 * Get the transaction mode
	 * @return true if the auto-commit mode is active
	 */
	public boolean isAutoCommit() {
		return chkAutoCommit.getSelection();
	}

}
