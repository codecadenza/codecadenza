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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.TRANSLATION_KEYS_CLASS;
import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;

import net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Generator for internationalization code
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFI18NGenerator extends AbstractI18NGenerator {
	/**
	 * Constructor
	 * @param project
	 */
	public JSFI18NGenerator(Project project) {
		super(project);
	}

	/**
	 * @param column
	 * @param formName
	 * @return the generated content
	 */
	public String getI18NBundleFragment(TableColumnField column, String formName) {
		final DomainAttribute domainAttr = column.getDTOAttribute().getDomainAttribute();
		final var defaultKey = LBL_DOMAIN_ATTR_PREFIX + domainAttr.getDomainObject().getName() + "_" + domainAttr.getName();
		final var key = COL_PREFIX + formName + "_" + column.getDTOAttribute().getName();

		// If the column title and the default label are equal we won't create a separate translation entry!
		if (domainAttr.getGUILabel().equals(column.getTitle()))
			return getBundleFragment(defaultKey, domainAttr.getGUILabel());

		return getBundleFragment(key, column.getTitle());
	}

	/**
	 * @param form
	 * @return the generated content
	 */
	public String getI18NBundleFragment(Form form) {
		final String key = FORM_PREFIX + form.getName() + TITLE_SUFFIX;

		return getBundleFragment(key, form.getTitle());
	}

	/**
	 * @param dto
	 * @return the generated content
	 */
	public String getI18NBundleFragment(DTOBean dto) {
		final String key = DOMAIN_OBJ_PREFIX + dto.getDomainObject().getName();

		return getBundleFragment(key, dto.getDomainObject().getLabel());
	}

	/**
	 * @param group
	 * @return the generated content
	 */
	public String getI18NBundleFragment(FormGroup group) {
		String key = FORM_GROUP_PREFIX;

		if (group.getParentGroup() == null)
			key += TOP_LEVEL_FORM_GROUP;
		else
			key += getConstantNameForKey(group.getParentGroup().getName());

		key += "_" + getConstantNameForKey(group.getName());

		return getBundleFragment(key, group.getName());
	}

	/**
	 * @param key
	 * @param msg
	 * @return the bundle fragment code
	 */
	public String getBundleFragment(String key, String msg) {
		propertiesEN.put(key.toLowerCase(), msg);

		return "bundle.getString(" + getConstantNameForKey(key) + ")";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragment(java.lang.String)
	 */
	@Override
	public String getCodeFragment(String key) {
		return "#{" + EL_I18N_VAR + "." + key.toLowerCase() + "}";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#
	 * getCodeFragmentForFieldLabel(java.lang.String)
	 */
	@Override
	public String getCodeFragmentForFieldLabel(String key) {
		throw new UnsupportedOperationException(
				"The method getCodeFragmentForFieldLabel() hasn't been implemented for JSF generators!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragmentWithParam(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public String getCodeFragmentWithParam(String key, String param) {
		throw new UnsupportedOperationException("The method getCodeFragmentWithParam() hasn't been implemented for JSF generators!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#createI18NService()
	 */
	@Override
	public void createI18NService() throws Exception {
		final var b = new StringBuilder();
		b.append("public interface " + TRANSLATION_KEYS_CLASS + "\n");
		b.append("{\n");

		// Create constants for all translation keys
		getTranslationKeys().forEach(key -> b.append("String " + getConstantNameForKey(key) + " = \"" + key + "\";\n"));

		b.append("}\n");

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, TRANSLATION_KEYS_CLASS,
				project.getClientNamespace().toString());
		javaFile.setComment("Interface that holds constants for all translation keys");
		javaFile.setContent(b.toString());

		EclipseIDEService.createJavaFile(javaFile);
	}

}
