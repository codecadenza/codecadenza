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

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.BUNDLE_NAME;

import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Implementation of an I18N generator for Swing, JavaFX, Vaadin and Eclipse RCP/RAP
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RichClientI18NGenerator extends AbstractI18NGenerator {
	/**
	 * Constructor
	 * @param project
	 */
	public RichClientI18NGenerator(Project project) {
		super(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragment(java.lang.String)
	 */
	@Override
	public String getCodeFragment(String key) {
		return "getTranslation(" + getConstantNameForKey(key) + ")";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragmentForFieldLabel(java.lang.String)
	 */
	@Override
	public String getCodeFragmentForFieldLabel(String key) {
		return "getTranslationForFieldLabel(" + getConstantNameForKey(key) + ")";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#getCodeFragmentWithParam(java.lang.String,
	 * java.lang.String)
	 */
	@Override
	public String getCodeFragmentWithParam(String key, String param) {
		return "getTranslation(" + getConstantNameForKey(key) + ", " + param + ")";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.i18n.AbstractI18NGenerator#createI18NService()
	 */
	@Override
	public void createI18NService() throws Exception {
		final var b = new StringBuilder();
		final var comment = "Utility class for translation of text fragments within this application";

		b.append("import net.codecadenza.runtime.i18n.*;\n");
		b.append("import java.util.*;\n\n");
		b.append("public class " + APP_I18N_PROVIDER_CLASS + "\n");
		b.append("{\n");

		// Create constants for all translation keys
		getTranslationKeys()
				.forEach(key -> b.append("public static final String " + getConstantNameForKey(key) + " = \"" + key + "\";\n"));

		b.append("private static final String BUNDLE_NAME = \"" + BUNDLE_NAME + "\";\n");
		b.append("private static final ResourceBundle bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.ENGLISH);\n\n");
		b.append("/**\n");
		b.append(" * Prevent instantiation\n");
		b.append(" */\n");
		b.append("private I18NApp()\n");
		b.append("{\n\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param key\n");
		b.append(" * @return the translation text by using the given key\n");
		b.append(" */\n");
		b.append("public static String getTranslation(String key)\n");
		b.append("{\n");
		b.append("return I18N.getTranslation(bundle, key);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param key\n");
		b.append(" * @param params\n");
		b.append(" * @return the translation text by using the given key\n");
		b.append(" */\n");
		b.append("public static String getTranslation(String key, Object... params)\n");
		b.append("{\n");
		b.append("return I18N.getTranslation(bundle, key, params);\n");
		b.append("}\n\n");
		b.append("/**\n");
		b.append(" * @param key\n");
		b.append(" * @return the translation for a field label by using the given key\n");
		b.append(" */\n");
		b.append("public static String getTranslationForFieldLabel(String key)\n");
		b.append("{\n");
		b.append("return I18N.getTranslationForFieldLabel(bundle, key);\n");
		b.append("}\n");
		b.append("}\n");

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, APP_I18N_PROVIDER_CLASS,
				project.getClientNamespace().toString());
		javaFile.setComment(comment);
		javaFile.setContent(b.toString());

		EclipseIDEService.createJavaFile(javaFile);
	}

}
