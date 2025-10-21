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
package net.codecadenza.eclipse.buildtest.dialog;

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;
import net.codecadenza.eclipse.buildtest.mapping.ConfigurationGroup;
import net.codecadenza.eclipse.buildtest.mapping.ConfigurationGroupRoot;
import net.codecadenza.eclipse.buildtest.mapping.ProjectConfiguration;
import net.codecadenza.eclipse.model.project.BuildToolEnumeration;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.project.TechnologyPlatformEnumeration;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.runtime.richclient.eclipse.widget.CheckboxDataGridComposite;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for selecting project(s) to be (re)created
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectSelectionDialog extends CodeCadenzaDialog {
	private CheckboxDataGridComposite<ProjectConfiguration> tabProjects;
	private final ConfigurationGroupRoot configRoot;
	private List<ProjectConfiguration> selectedProjects;
	private DataComboViewer<ConfigurationGroup> cboConfigGroup;
	private DataComboViewer<BuildToolEnumeration> cboBuildTool;
	private Button chkProtectManualChanges;
	private Button chkAddREST;
	private Button chkAddSOAP;
	private Button chkAddRMI;
	private Button chkAddKafka;
	private Button chkAddJMS;
	private Button chkAddSelenium;
	private Button chkAddIntegrationTests;
	private boolean protectManualChanges;
	private boolean addREST = true;
	private boolean addSOAP = true;
	private boolean addRMI = true;
	private boolean addKafka = true;
	private boolean addJMS = true;
	private boolean addSelenium = true;
	private boolean addIntegrationTests = true;
	private boolean selectAll = true;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param configRoot
	 */
	public ProjectSelectionDialog(Shell parentShell, ConfigurationGroupRoot configRoot) {
		super(parentShell);

		this.configRoot = configRoot;
	}

	/**
	 * @return a list containing all projects to be created
	 */
	public List<ProjectConfiguration> getSelectedProjects() {
		selectedProjects.forEach(config -> {
			final TechnologyPlatformEnumeration technology = config.getTechnologyPlatform();

			config.setProtectManualChanges(protectManualChanges);
			config.setAddREST(false);
			config.setAddSOAP(false);
			config.setAddRMI(false);
			config.setAddKafka(false);
			config.setAddJMS(false);
			config.setAddSelenium(false);
			config.setAddIntegrationTests(false);

			// Accept the user selection only if the selected configuration basically supports a given technology!
			if (technology == TechnologyPlatformEnumeration.SPRING_BOOT || technology == TechnologyPlatformEnumeration.JAKARTA_EE) {
				config.setAddREST(addREST);
				config.setAddSOAP(addSOAP);
				config.setAddJMS(addJMS);
				config.setAddIntegrationTests(addIntegrationTests);

				if (technology == TechnologyPlatformEnumeration.JAKARTA_EE)
					config.setAddRMI(addRMI);
				else if (config.isBoundaryMode())
					config.setAddKafka(addKafka);
			}

			if (config.getClientPlatform() == ClientPlatformEnumeration.ANGULAR
					|| config.getClientPlatform() == ClientPlatformEnumeration.JSF_PRIMEFACES
					|| config.getClientPlatform() == ClientPlatformEnumeration.VAADIN)
				config.setAddSelenium(addSelenium);

			if (config.getClientPlatform() == ClientPlatformEnumeration.ANGULAR)
				config.setAddREST(true);
		});

		return selectedProjects;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 5);

		final var lblMode = new Label(panDialogArea, SWT.NONE);
		lblMode.setText("Select the client technology:");

		cboConfigGroup = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(ConfigurationGroup element) {
				return element.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
			 */
			@Override
			public void onSelectionChanged(ConfigurationGroup element) {
				tabProjects.setData(getConfigurations());
			}
		};

		final var lblBuildTool = new Label(panDialogArea, SWT.NONE);
		lblBuildTool.setText("Select the build tool:");

		cboBuildTool = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(BuildToolEnumeration element) {
				return element.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
			 */
			@Override
			public void onSelectionChanged(BuildToolEnumeration element) {
				tabProjects.setData(getConfigurations());
			}
		};

		chkProtectManualChanges = new Button(panDialogArea, SWT.CHECK);
		chkProtectManualChanges.setText("Protect manual changes");

		chkProtectManualChanges.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				protectManualChanges = chkProtectManualChanges.getSelection();
			}
		});

		final GridLayout glOptionalArtifacts = new GridLayout(9, false);
		glOptionalArtifacts.marginWidth = 0;

		final var panOptionalArtifacts = new Composite(panDialogArea, SWT.NONE);
		panOptionalArtifacts.setLayoutData(new GridData(SWT.FILL, SWT.LEFT, true, false, 5, 1));
		panOptionalArtifacts.setLayout(glOptionalArtifacts);

		final var lblOptionalArtifacts = new Label(panOptionalArtifacts, SWT.NONE);
		lblOptionalArtifacts.setText("Include optional artifacts:");

		chkAddREST = new Button(panOptionalArtifacts, SWT.CHECK);
		chkAddREST.setSelection(true);
		chkAddREST.setText("REST");

		chkAddREST.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addREST = chkAddREST.getSelection();
			}
		});

		chkAddSOAP = new Button(panOptionalArtifacts, SWT.CHECK);
		chkAddSOAP.setSelection(true);
		chkAddSOAP.setText("SOAP");

		chkAddSOAP.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addSOAP = chkAddSOAP.getSelection();
			}
		});

		chkAddRMI = new Button(panOptionalArtifacts, SWT.CHECK);
		chkAddRMI.setSelection(true);
		chkAddRMI.setText("RMI");

		chkAddRMI.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addRMI = chkAddRMI.getSelection();
			}
		});

		chkAddKafka = new Button(panOptionalArtifacts, SWT.CHECK);
		chkAddKafka.setSelection(true);
		chkAddKafka.setText("Kafka");

		chkAddKafka.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addKafka = chkAddKafka.getSelection();
			}
		});

		chkAddJMS = new Button(panOptionalArtifacts, SWT.CHECK);
		chkAddJMS.setSelection(true);
		chkAddJMS.setText("JMS");

		chkAddJMS.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addJMS = chkAddJMS.getSelection();
			}
		});

		chkAddSelenium = new Button(panOptionalArtifacts, SWT.CHECK);
		chkAddSelenium.setSelection(true);
		chkAddSelenium.setText("Selenium");

		chkAddSelenium.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addSelenium = chkAddSelenium.getSelection();
			}
		});

		chkAddIntegrationTests = new Button(panOptionalArtifacts, SWT.CHECK);
		chkAddIntegrationTests.setSelection(true);
		chkAddIntegrationTests.setText("Integration tests");

		chkAddIntegrationTests.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addIntegrationTests = chkAddIntegrationTests.getSelection();
			}
		});

		final var cmdToggleSelection = new Button(panOptionalArtifacts, SWT.NONE);
		cmdToggleSelection.setText("Toggle");

		cmdToggleSelection.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectAll = !selectAll;
				addREST = selectAll;
				addSOAP = selectAll;
				addRMI = selectAll;
				addKafka = selectAll;
				addJMS = selectAll;
				addSelenium = selectAll;
				addIntegrationTests = selectAll;

				chkAddREST.setSelection(selectAll);
				chkAddSOAP.setSelection(selectAll);
				chkAddRMI.setSelection(selectAll);
				chkAddKafka.setSelection(selectAll);
				chkAddJMS.setSelection(selectAll);
				chkAddSelenium.setSelection(selectAll);
				chkAddIntegrationTests.setSelection(selectAll);
			}
		});

		tabProjects = new CheckboxDataGridComposite<>(panDialogArea, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(ProjectConfiguration element, int columnIndex) {
				if (columnIndex == 0)
					return element.getProjectName();
				else if (columnIndex == 1)
					return element.getTechnologyPlatform().name();
				else if (columnIndex == 2)
					return element.getServerPlatform().name();
				else if (columnIndex == 3)
					return element.getPersistenceProvider().name();

				return "";
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellImage(java.lang.Object, int)
			 */
			@Override
			public Image getCellImage(ProjectConfiguration element, int columnIndex) {
				if (columnIndex == 4) {
					if (element.isBoundaryMode())
						return CodeCadenzaResourcePlugin.getImage("checked.gif");

					return CodeCadenzaResourcePlugin.getImage("unchecked.gif");
				}

				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellForeground(java.lang. Object,
			 * int)
			 */
			@Override
			public Color getCellForeground(ProjectConfiguration element, int columnIndex) {
				if (element.getTechnologyPlatform() == TechnologyPlatformEnumeration.JAKARTA_EE)
					return Display.getDefault().getSystemColor(SWT.COLOR_DARK_GREEN);
				else if (element.getTechnologyPlatform() == TechnologyPlatformEnumeration.SPRING_BOOT)
					return Display.getDefault().getSystemColor(SWT.COLOR_DARK_BLUE);

				return null;
			}
		};

		final var gdProjects = new GridData(SWT.FILL, SWT.FILL, true, true, 5, 1);
		gdProjects.widthHint = 800;
		gdProjects.heightHint = 600;

		tabProjects.setLayoutData(gdProjects);
		tabProjects.addColumn("Project name", ColumnSortType.STRING, 330);
		tabProjects.addColumn("Technology", ColumnSortType.STRING, 120);
		tabProjects.addColumn("Platform", ColumnSortType.STRING, 100);
		tabProjects.addColumn("JPA provider", ColumnSortType.STRING, 100);
		tabProjects.addColumn("Boundary mode", 120);
		tabProjects.getTableViewer().getTable().setLinesVisible(false);

		// Sort the project configurations of all groups!
		for (final ConfigurationGroup confGroup : configRoot.getConfigurationGroups())
			confGroup.getConfigurations().sort((pr1, pr2) -> {
				int val1 = 0;
				int val2 = 0;

				if (pr1.getTechnologyPlatform() == TechnologyPlatformEnumeration.SPRING_BOOT)
					val1 = 1;
				else if (pr1.getTechnologyPlatform() == TechnologyPlatformEnumeration.JAVA_SE)
					val1 = 2;

				if (pr2.getTechnologyPlatform() == TechnologyPlatformEnumeration.SPRING_BOOT)
					val2 = 1;
				else if (pr2.getTechnologyPlatform() == TechnologyPlatformEnumeration.JAVA_SE)
					val2 = 2;

				return val1 - val2;
			});

		cboConfigGroup.setData(configRoot.getConfigurationGroups());
		cboConfigGroup.setSelectedItem(configRoot.getConfigurationGroups().get(0));

		if (EclipseIDEService.isM2EInstalled()) {
			cboBuildTool.setData(Stream.of(BuildToolEnumeration.values()).toList());
			cboBuildTool.setSelectedItem(BuildToolEnumeration.MAVEN);
		}

		return panDialogArea;
	}

	/**
	 * @return a list of project configuration objects based on the selected user input
	 */
	private List<ProjectConfiguration> getConfigurations() {
		final var items = new ArrayList<ProjectConfiguration>();
		final ConfigurationGroup configGroup = cboConfigGroup.getSelectedItem();
		final BuildToolEnumeration selectedBuildTool = cboBuildTool.getSelectedItem();

		if (selectedBuildTool == null)
			return items;

		for (final ProjectConfiguration config : configGroup.getConfigurations())
			if (config.getBuildTool() == selectedBuildTool)
				items.add(config);

		return items;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Select projects to be (re)created");
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		selectedProjects = tabProjects.getCheckedElements();

		super.buttonPressed(buttonId);
	}

}
