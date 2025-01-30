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
package net.codecadenza.eclipse.shared.dialog;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Base class for all internal dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class CodeCadenzaDialog extends Dialog {
	/**
	 * Create a dialog instance
	 * @param parentShell the parent shell, or <code>null</code> to create a top-level shell
	 */
	protected CodeCadenzaDialog(Shell parentShell) {
		super(parentShell);

		this.setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL | SWT.RESIZE);
	}

	/**
	 * Create a dialog with the given parent
	 * @param parentShell object that returns the current parent shell
	 */
	protected CodeCadenzaDialog(IShellProvider parentShell) {
		super(parentShell);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		// Create a dialog area with one grid column and add a separator above the button area
		return createDialogArea(parent, 1, true);
	}

	/**
	 * Create the area of the dialog. The composite that is returned uses a <code>GridLayout</code>.
	 * @param parent the parent composite to contain the dialog area
	 * @param gridColumns the number of columns of the <code>GridLayout</code>
	 * @return the control that represents the dialog area
	 */
	protected Control createDialogArea(Composite parent, int gridColumns) {
		// Create a dialog area with an arbitrary number of grid columns and add a separator above the button area
		return createDialogArea(parent, gridColumns, true);
	}

	/**
	 * Create the area of the dialog. The composite that is returned uses a <code>GridLayout</code>.
	 * @param parent the parent composite to contain the dialog area
	 * @param gridColumns the number of columns of the <code>GridLayout</code>
	 * @param addSeparator controls if a separator above of the button area should be added
	 * @return the control that represents the dialog area
	 */
	protected Control createDialogArea(Composite parent, int gridColumns, boolean addSeparator) {
		// Create the full dialog area with standard margins
		final var panFullArea = (Composite) super.createDialogArea(parent);

		// Set all margins to 0 in order to avoid too much space between the dialog border and the components being added!
		final var glDialogArea = new GridLayout(gridColumns, false);
		glDialogArea.marginWidth = 0;
		glDialogArea.marginHeight = 0;

		// Create the dialog area that can be used as a container for all controls
		final var panDialogArea = new Composite(panFullArea, SWT.NONE);
		panDialogArea.setLayout(glDialogArea);
		panDialogArea.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		if (addSeparator) {
			// Add a separator above the button area
			final var separator = new Label(parent, SWT.HORIZONTAL | SWT.SEPARATOR);
			separator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		}

		return panDialogArea;
	}

}
