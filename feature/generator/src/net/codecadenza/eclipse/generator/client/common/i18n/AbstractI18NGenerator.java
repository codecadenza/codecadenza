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
package net.codecadenza.eclipse.generator.client.common.i18n;

import static net.codecadenza.eclipse.shared.Constants.ADMIN_PANEL_TITLE;
import static net.codecadenza.eclipse.shared.Constants.BASIC_PANEL_TITLE;
import static net.codecadenza.eclipse.shared.Constants.TRANSLATION_FILE_NAME;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;
import java.util.TreeSet;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.generator.CodeCadenzaGeneratorPlugin;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;

/**
 * <p>
 * Abstract base class that provides common internationalization features for all client technologies
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractI18NGenerator {
	protected static final String DOMAIN_OBJ_PREFIX = "domain_obj_";
	protected static final String LBL_DOMAIN_ATTR_PREFIX = "lbl_attr_";
	protected static final String LBL_DOMAIN_ASSOC_PREFIX = "lbl_assoc_";
	protected static final String COM_DOMAIN_ATTR_PREFIX = "com_attr_";
	protected static final String COM_DOMAIN_ASSOC_PREFIX = "com_assoc_";
	protected static final String FIELD_PREFIX = "field_";
	protected static final String FORM_PREFIX = "form_";
	protected static final String PANEL_PREFIX = "panel_";
	protected static final String COL_PREFIX = "col_";
	protected static final String FORM_GROUP_PREFIX = "fg_";
	protected static final String TITLE_SUFFIX = "_title";
	protected static final String TOP_LEVEL_FORM_GROUP = "top";
	protected static final String ADMIN_PANEL_KEY = PANEL_PREFIX + "admin" + TITLE_SUFFIX;
	protected static final String BASIC_PANEL_KEY = PANEL_PREFIX + "basic" + TITLE_SUFFIX;
	private static final Pattern UNDERSCORE_PATTERN = Pattern.compile("[_]{2,}");

	protected Properties propertiesEN = new Properties();
	protected Project project;
	private IFile translationFile;
	private IProject workspaceProject;

	/**
	 * Constructor
	 * @param project
	 */
	protected AbstractI18NGenerator(Project project) {
		this.project = project;

		final IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		final String path = getTranslationFilePath();

		try {
			workspaceProject = wsRoot.getProject(project.getTargetProjectName(BuildArtifactType.GUI));
			translationFile = workspaceProject.getFile(path);
		}
		catch (final Exception e) {
			CodeCadenzaGeneratorPlugin.getInstance().logError(e);
			return;
		}

		try (InputStream inputStream = translationFile.getContents()) {
			// Load the translations from the respective file
			propertiesEN.load(inputStream);
		}
		catch (final Exception e) {
			CodeCadenzaGeneratorPlugin.getInstance().logError(e);
		}
	}

	/**
	 * @return the path to the translation file
	 */
	public String getTranslationFilePath() {
		return "/" + project.getResourceFolder() + "/" + TRANSLATION_FILE_NAME;
	}

	/**
	 * @param key
	 * @return the generated content
	 */
	public abstract String getCodeFragment(String key);

	/**
	 * @param key
	 * @return the generated content
	 */
	public abstract String getCodeFragmentForFieldLabel(String key);

	/**
	 * @param key
	 * @param param
	 * @return the generated content
	 */
	public abstract String getCodeFragmentWithParam(String key, String param);

	/**
	 * Create the class that provides internationalization features
	 * @throws Exception if an internal error has occurred
	 */
	public abstract void createI18NService() throws Exception;

	/**
	 * Save new or changed translations
	 * @throws IllegalStateException if the translation file wasn't initialized
	 */
	public void save() {
		if (translationFile == null)
			throw new IllegalStateException("Error while performing the save operation! The translation file wasn't initialized!");

		try {
			final var outputStream = new ByteArrayOutputStream();

			// Override the method keys() in order to sort the properties when saving them
			final var sortedProperties = new Properties() {
				private static final long serialVersionUID = 7355402399242916232L;

				/*
				 * (non-Javadoc)
				 * @see java.util.Hashtable#keys()
				 */
				@Override
				public synchronized Enumeration<Object> keys() {
					return Collections.enumeration(new TreeSet<>(super.keySet()));
				}
			};

			sortedProperties.putAll(propertiesEN);
			sortedProperties.store(outputStream, null);

			translationFile.setContents(new ByteArrayInputStream(outputStream.toByteArray()), true, false, null);

			createI18NService();
		}
		catch (final Exception e) {
			CodeCadenzaGeneratorPlugin.getInstance().logError(e);
		}
	}

	/**
	 * @param field
	 * @param addLabelSeparator
	 * @return the generated content
	 */
	public String getI18N(FormField field, boolean addLabelSeparator) {
		final var key = FIELD_PREFIX + field.getPanel().getForm().getName() + "_" + field.getName();
		final DTOBeanAttribute dtoAttr = field.getDTOAttribute();
		final String defaultKey;

		if (field.getDTOAttribute().getDomainAttribute() != null) {
			defaultKey = LBL_DOMAIN_ATTR_PREFIX + dtoAttr.getDomainAttribute().getDomainObject().getName() + "_"
					+ dtoAttr.getDomainAttribute().getName();
			propertiesEN.put(defaultKey.toLowerCase(), dtoAttr.getDomainAttribute().getGUILabel());
		}
		else {
			defaultKey = LBL_DOMAIN_ASSOC_PREFIX + dtoAttr.getAssociation().getDomainObject().getName() + "_"
					+ dtoAttr.getAssociation().getName();
			propertiesEN.put(defaultKey.toLowerCase(), dtoAttr.getAssociation().getGUILabel());
		}

		// If the field and the default label are equal we won't create a separate translation entry!
		if (propertiesEN.get(defaultKey.toLowerCase()).equals(field.getLabel())) {
			if (addLabelSeparator)
				return getCodeFragmentForFieldLabel(defaultKey.toLowerCase());

			return getCodeFragment(defaultKey.toLowerCase());
		}

		propertiesEN.put(key.toLowerCase(), field.getLabel());

		if (addLabelSeparator)
			return getCodeFragmentForFieldLabel(key.toLowerCase());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param field
	 * @return the generated content
	 */
	public String getI18N(FormField field) {
		return getI18N(field, false);
	}

	/**
	 * @param column
	 * @return the generated content
	 */
	public String getI18N(TableColumnField column) {
		final DomainAttribute domainAttr = column.getDTOAttribute().getDomainAttribute();
		final var defaultKey = LBL_DOMAIN_ATTR_PREFIX + domainAttr.getDomainObject().getName() + "_" + domainAttr.getName();
		var name = "";

		propertiesEN.put(defaultKey.toLowerCase(), domainAttr.getGUILabel());

		// If the column title and the default label are equal we won't create a separate translation entry!
		if (domainAttr.getGUILabel().equals(column.getTitle()))
			return getCodeFragment(defaultKey.toLowerCase());

		if (column.getFormTable().getFormPanel().getForm() == null)
			name = column.getFormTable().getFormPanel().getName();
		else
			name = column.getFormTable().getFormPanel().getForm().getName();

		final var key = COL_PREFIX + name + "_" + column.getDTOAttribute().getName();

		propertiesEN.put(key.toLowerCase(), column.getTitle());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param form
	 * @return the generated content
	 */
	public String getI18N(Form form) {
		final String key = FORM_PREFIX + form.getName() + TITLE_SUFFIX;

		propertiesEN.put(key.toLowerCase(), form.getTitle());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param group
	 * @return the generated content
	 */
	public String getI18N(FormGroup group) {
		String key = FORM_GROUP_PREFIX;

		if (group.getParentGroup() == null)
			key += TOP_LEVEL_FORM_GROUP;
		else
			key += getConstantNameForKey(group.getParentGroup().getName());

		key += "_" + getConstantNameForKey(group.getName());

		propertiesEN.put(key.toLowerCase(), group.getName());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param panel
	 * @return the generated content
	 */
	public String getI18N(FormPanel panel) {
		String key = PANEL_PREFIX;

		if (panel.getForm() != null)
			key += panel.getForm().getName() + "_";

		key += panel.getName() + TITLE_SUFFIX;

		propertiesEN.computeIfAbsent(ADMIN_PANEL_KEY, translationKey -> ADMIN_PANEL_TITLE);
		propertiesEN.computeIfAbsent(BASIC_PANEL_KEY, translationKey -> BASIC_PANEL_TITLE);

		// Don't create a separate translation entry if the panel has a default title!
		if (propertiesEN.get(ADMIN_PANEL_KEY).equals(panel.getLabel()))
			return getCodeFragment(ADMIN_PANEL_KEY);

		if (propertiesEN.get(BASIC_PANEL_KEY).equals(panel.getLabel()))
			return getCodeFragment(BASIC_PANEL_KEY);

		propertiesEN.put(key.toLowerCase(), panel.getLabel());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param dto
	 * @return the generated content
	 */
	public String getI18N(DTOBean dto) {
		final String key = DOMAIN_OBJ_PREFIX + dto.getDomainObject().getName();

		propertiesEN.put(key.toLowerCase(), dto.getDomainObject().getLabel());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param dto
	 * @param msg
	 * @return the generated content
	 */
	public String getI18N(DTOBean dto, String msg) {
		final String key = dto.getName();

		propertiesEN.put(key.toLowerCase(), msg);

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param assoc
	 * @return the generated content
	 */
	public String getI18N(AbstractDomainAssociation assoc) {
		final var key = COM_DOMAIN_ASSOC_PREFIX + assoc.getDomainObject().getName() + "_" + assoc.getName();

		propertiesEN.put(key.toLowerCase(), assoc.getUserComment());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param attribute
	 * @return the generated content
	 */
	public String getI18N(DomainAttribute attribute) {
		final var key = COM_DOMAIN_ATTR_PREFIX + attribute.getDomainObject().getName() + "_" + attribute.getName();

		propertiesEN.put(key.toLowerCase(), attribute.getUserComment());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param dto
	 * @return the generated content
	 */
	public String getI18NPlural(DTOBean dto) {
		final var key = DOMAIN_OBJ_PREFIX + dto.getDomainObject().getLabelPlural().replace(" ", "_");

		propertiesEN.put(key.toLowerCase(), dto.getDomainObject().getLabelPlural());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param panel
	 * @param action
	 * @return the generated content
	 */
	public String getI18N(FormPanel panel, FormAction action) {
		final var key = panel.getName() + "_" + action.getName();

		propertiesEN.put(key.toLowerCase(), action.getDescription());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param action
	 * @return the generated content
	 */
	public String getI18N(FormAction action) {
		final var key = action.getForm().getName() + "_" + action.getName();

		propertiesEN.put(key.toLowerCase(), action.getDescription());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param attr
	 * @param msg
	 * @param addLabelSeparator
	 * @return the generated content
	 */
	public String getI18N(DTOBeanAttribute attr, String msg, boolean addLabelSeparator) {
		final var key = attr.getDTOBean().getName() + "_" + attr.getName();

		propertiesEN.put(key.toLowerCase(), msg);

		if (addLabelSeparator)
			return getCodeFragmentForFieldLabel(key.toLowerCase());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param attr
	 * @return the generated content
	 */
	public String getI18N(DTOBeanAttribute attr) {
		return getI18N(attr, attr.getDomainAttribute().getGUILabel(), false);
	}

	/**
	 * @param attr
	 * @param msg
	 * @return the generated content
	 */
	public String getI18N(DTOBeanAttribute attr, String msg) {
		return getI18N(attr, msg, false);
	}

	/**
	 * @param key
	 * @param msg
	 * @param addLabelSeparator
	 * @return the generated content
	 */
	public String getI18NMessage(String key, String msg, boolean addLabelSeparator) {
		propertiesEN.put(key.toLowerCase(), msg);

		if (addLabelSeparator)
			return getCodeFragmentForFieldLabel(key);

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @param key
	 * @param msg
	 * @return the generated content
	 */
	public String getI18NMessage(String key, String msg) {
		return getI18NMessage(key, msg, false);
	}

	/**
	 * @param key
	 * @param msg
	 * @param param
	 * @return the generated content
	 */
	public String getI18NMessage(String key, String msg, String param) {
		propertiesEN.put(key.toLowerCase(), msg);

		return getCodeFragmentWithParam(key.toLowerCase(), param);
	}

	/**
	 * @param literal
	 * @return the generated content
	 */
	public String getI18N(EnumLiteral literal) {
		final var key = literal.getJavaEnum().getName() + "_" + literal.getName();

		propertiesEN.put(key.toLowerCase(), literal.getName());

		return getCodeFragment(key.toLowerCase());
	}

	/**
	 * @return a sorted list that contains all translation keys
	 */
	protected List<String> getTranslationKeys() {
		return propertiesEN.keySet().stream().map(String.class::cast).sorted(String::compareTo).toList();
	}

	/**
	 * @param key
	 * @return the name of the Java constant for the corresponding key
	 */
	protected String getConstantNameForKey(String key) {
		final var constant = new StringBuilder();
		boolean replace = false;

		for (int index = 0; index < key.length(); index++) {
			replace = false;

			if (index == 0 && !Character.isJavaIdentifierStart(key.charAt(index)))
				replace = true;

			if (index > 0 && !Character.isJavaIdentifierPart(key.charAt(index)))
				replace = true;

			// Replace all illegal characters with an underscore
			if (replace)
				constant.append("_");
			else
				constant.append(key.charAt(index));
		}

		// Remove unnecessary underscores and convert all letters to upper-case
		return UNDERSCORE_PATTERN.matcher(constant.toString()).replaceAll("_").toUpperCase();
	}

}
