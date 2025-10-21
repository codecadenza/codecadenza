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

import java.nio.file.FileSystems;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Panel for test data attributes that are mapped to either a file or a binary content
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FileChooserTestDataAttributePanel extends AbstractTestDataAttributePanel {
	private static final Pattern PATH_PATTERN = Pattern.compile(Pattern.quote(FileSystems.getDefault().getSeparator()));

	private Text txtPath;

	/**
	 * Constructor
	 * @param initializationData
	 */
	public FileChooserTestDataAttributePanel(TestDataAttributePanelData initializationData) {
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

		final var gdPanFile = new GridLayout(2, false);
		gdPanFile.marginWidth = 0;
		gdPanFile.marginHeight = 0;

		final var panFile = new Composite(this, SWT.NONE);
		panFile.setLayout(gdPanFile);
		panFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		txtPath = new Text(panFile, SWT.BORDER);
		txtPath.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (testDataAttribute.getValue() != null)
			txtPath.setText(testDataAttribute.getValue());

		setBackgroundColor(txtPath);

		final var cmdBrowse = new Button(panFile, SWT.NONE);
		cmdBrowse.setText("Browse");
		cmdBrowse.setEnabled(isEditable());

		cmdBrowse.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final Shell shell = Display.getCurrent().getActiveShell();

				final var dlg = new FileDialog(shell, SWT.OPEN | SWT.SINGLE);
				dlg.setText("Select a file for upload");
				dlg.setFilterPath(getTestResourcePath().toOSString());

				final String pathToFile = dlg.open();

				if (pathToFile != null) {
					final IPath testResourcePath = getTestResourcePath();

					// Check if the relative path of the test resource folder can be used
					if (pathToFile.startsWith(testResourcePath.toOSString())) {
						// Extract the relative path from the 'src/test/resources' path
						final String relativePath = pathToFile.replaceFirst(testResourcePath.toOSString(), "");

						// Replace all system-dependent path separator characters (e.g. '\' on Windows)
						final Matcher matcher = PATH_PATTERN.matcher(relativePath);
						final String resourcePath = matcher.replaceAll("/");
						final String filePath = project.getTestResourceFolder() + (resourcePath.startsWith("/") ? "" : "/") + resourcePath;

						txtPath.setText(filePath);
					}
					else
						txtPath.setText(pathToFile);
				}
				else
					txtPath.setText("");
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.dialog.testing.integration.panel.attribute.AbstractTestDataAttributePanel#
	 * validateAndApplyInput()
	 */
	@Override
	public void validateAndApplyInput() {
		testDataAttribute.setOperator(getSelectedOperator());

		if (txtPath.getText().isEmpty())
			testDataAttribute.setValue(null);
		else
			testDataAttribute.setValue(txtPath.getText());
	}

	/**
	 * @return the path of the project's test resources folder
	 */
	private IPath getTestResourcePath() {
		final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		final String testProjectName = project.getTargetProjectName(testModule.getArtifactType());
		final IProject existingProject = workspaceRoot.getProject(testProjectName);

		return existingProject.getLocation().append(project.getTestResourceFolder());
	}

}
