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
package net.codecadenza.eclipse.ui.panel;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import org.eclipse.emf.common.util.EList;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.List;

/**
 * <p>
 * Panel to maintain JDBC path entries
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DriverSelectionPanel extends Composite {
	private List listExtLibraries;

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 * @param driverList
	 */
	public DriverSelectionPanel(Composite parent, int style, EList<String> driverList) {
		super(parent, style);

		final var glExtLibraries = new GridLayout(2, false);
		glExtLibraries.marginWidth = 0;

		setLayout(glExtLibraries);

		final var scrolledComposite = new ScrolledComposite(this, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
		scrolledComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		scrolledComposite.setExpandHorizontal(true);
		scrolledComposite.setExpandVertical(true);

		listExtLibraries = new List(scrolledComposite, SWT.H_SCROLL | SWT.V_SCROLL);

		scrolledComposite.setContent(listExtLibraries);
		scrolledComposite.setMinSize(listExtLibraries.computeSize(SWT.DEFAULT, SWT.DEFAULT));

		if (driverList != null)
			driverList.forEach(listExtLibraries::add);

		final var panExtLibButtons = new Composite(this, SWT.NONE);
		panExtLibButtons.setLayoutData(new GridData(SWT.CENTER, SWT.TOP, false, false));
		panExtLibButtons.setLayout(new GridLayout());

		final var cmdAddLib = new Button(panExtLibButtons, SWT.NONE);
		cmdAddLib.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		cmdAddLib.setText("Add");

		cmdAddLib.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlg = new FileDialog(Display.getCurrent().getActiveShell(), SWT.OPEN | SWT.MULTI);
				dlg.setFilterExtensions(new String[] { "*.jar", "*.zip" });
				dlg.setText("Select external JDBC library");

				final String selection = dlg.open();

				if (selection != null)
					for (final String name : dlg.getFileNames())
						listExtLibraries.add(dlg.getFilterPath() + File.separator + name);
			}
		});

		final var cmdRemoveLib = new Button(panExtLibButtons, SWT.NONE);
		cmdRemoveLib.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false));
		cmdRemoveLib.setText("Remove");

		cmdRemoveLib.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final int selectionIndex = listExtLibraries.getSelectionIndex();

				if (selectionIndex == -1)
					return;

				listExtLibraries.remove(selectionIndex);
			}
		});
	}

	/**
	 * @return a list containing the fully qualified paths of all selected libraries
	 */
	public java.util.List<String> getSelectedLibraries() {
		final var resultList = new ArrayList<String>();
		Collections.addAll(resultList, listExtLibraries.getItems());

		return resultList;
	}

}
