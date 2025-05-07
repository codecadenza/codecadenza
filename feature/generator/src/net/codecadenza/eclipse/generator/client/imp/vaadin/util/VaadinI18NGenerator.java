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
package net.codecadenza.eclipse.generator.client.imp.vaadin.util;

import net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Implementation of an I18N generator for Vaadin applications
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinI18NGenerator extends AbstractI18NGenerator {
	public static final String TRANSLATION_KEYS = "TranslationKeys";
	private static final String TRANSLATION_FILE_NAME = "translation_en.properties";
	private static final String LOCALE_FRAGMENT = "i18n.getLocale()";

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinI18NGenerator(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getTranslationFilePath()
	 */
	@Override
	public String getTranslationFilePath() {
		return "/" + project.getResourceFolder() + "/" + TRANSLATION_FILE_NAME;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragment(java.lang.String)
	 */
	@Override
	public String getCodeFragment(String key) {
		return "i18n.getTranslation(" + getConstantNameForKey(key) + ")";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#
	 * getCodeFragmentForFieldLabel(java.lang.String)
	 */
	@Override
	public String getCodeFragmentForFieldLabel(String key) {
		return "i18n.getTranslationForFieldLabel(" + getConstantNameForKey(key) + ")";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragmentWithParam(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public String getCodeFragmentWithParam(String key, String param) {
		return "i18n.getTranslation(" + getConstantNameForKey(key) + ", " + param + ")";
	}

	/**
	 * @return the code fragment for getting the locale
	 */
	public String getLocaleFragment() {
		return LOCALE_FRAGMENT;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#createI18NService()
	 */
	@Override
	public void createI18NService() throws Exception {
		final var b = new StringBuilder();
		final var comment = "Interface that holds all translation keys";

		b.append("public interface " + TRANSLATION_KEYS + "\n");
		b.append("{\n");

		// Create constants for all translation keys
		getTranslationKeys().forEach(key -> b.append("String " + getConstantNameForKey(key) + " = \"" + key + "\";\n"));

		b.append("}\n");

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, TRANSLATION_KEYS, project.getClientNamespace().toString());
		javaFile.setComment(comment);
		javaFile.setContent(b.toString());

		EclipseIDEService.createJavaFile(javaFile);
	}

}
